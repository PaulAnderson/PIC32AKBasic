#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

/* --- CONFIGURATION & MEMORY LIMITS --- */
#define POOL_SIZE       4096   /* Program memory pool in bytes */
#define MAX_LINES       128    /* Maximum program lines */
#define MAX_LINE_LEN    80     /* Input buffer length */
#define MAX_STRING_LEN  32     /* Max string variable length */
#define MAX_ARRAYS      26     /* A-Z Array support */
#define MAX_ARRAY_SIZE  100    /* Max elements per array */
#define MAX_GOSUB_DEPTH 8      /* GOSUB call stack limit */
#define MAX_FOR_DEPTH   8      /* FOR/NEXT loop stack limit */

enum Token {
    /* Keywords / Statements */
    TOKEN_PRINT = 0x80,
    TOKEN_LET,
    TOKEN_GOTO,
    TOKEN_IF,
    TOKEN_THEN,
    TOKEN_INPUT,
    TOKEN_END,
    TOKEN_LIST,
    TOKEN_RUN,
    TOKEN_CLEAR,
    TOKEN_FOR,
    TOKEN_TO,
    TOKEN_NEXT,
    TOKEN_STEP,
    TOKEN_GOSUB,
    TOKEN_RETURN,
    TOKEN_DIM,
    TOKEN_DATA,
    TOKEN_READ,
    TOKEN_RESTORE,
    TOKEN_POKE,
    TOKEN_PEEK,
    TOKEN_RND,
    TOKEN_ABS,
    TOKEN_SGN,
    TOKEN_CLAMP,
    TOKEN_BIT,
    TOKEN_BITREAD,
    TOKEN_BITSET,
    TOKEN_BITCLR,
    TOKEN_LEN,
    TOKEN_VAL,
    TOKEN_STR,
    TOKEN_LEFT,
    TOKEN_RIGHT,
    TOKEN_EQ,         /* == */
    TOKEN_NE,         /* <> or != */
    TOKEN_GE,         /* >= */
    TOKEN_LE,         /* <= */
};

typedef enum {
    VAR_TYPE_NUMERIC,
    VAR_TYPE_ARRAY,
    VAR_TYPE_STRING
} VarType;

typedef struct {
    uint16_t line_number;
    uint16_t offset;
    uint8_t  length;
} LineIndex;

typedef struct {
    int size;
    int data[MAX_ARRAY_SIZE];
} BasicArray;

typedef struct {
    int var_idx;
    int target_val;
    int step_val;
    int line_index;
} ForLoopFrame;

typedef struct {
    const char *keyword;
    uint8_t token;
    void (*handler)(const char **src);
} KeywordEntry;

typedef int (*FactorFuncHandler)(const char **src);

typedef struct {
    uint8_t token;
    FactorFuncHandler handler;
} FactorFuncEntry;

typedef struct {
    const char *name;
    uint32_t address;
} RegisterEntry;

/* --- GLOBAL STORAGE & INTERPRETER STATE --- */
static char program_pool[POOL_SIZE];
static LineIndex line_index_table[MAX_LINES];
static uint16_t pool_bytes_used = 0;
static uint16_t line_count = 0;

static int variables[26];
static char string_vars[26][MAX_STRING_LEN];
static BasicArray array_vars[MAX_ARRAYS];

static int current_exec_index = -1;
static int gosub_stack[MAX_GOSUB_DEPTH];
static int gosub_sp = 0;

static ForLoopFrame for_stack[MAX_FOR_DEPTH];
static int for_sp = 0;

static int data_line_idx = 0;
static int data_char_offset = 0;

/* --- PIC32 SPECIAL FUNCTION REGISTER (SFR) TABLE --- */
static const RegisterEntry SFR_TABLE[] = {
    {"TRISA",    0xBF886000}, {"TRISASET", 0xBF886004}, {"TRISACLR", 0xBF886008},
    {"TRISB",    0xBF886100}, {"TRISBSET", 0xBF886104}, {"TRISBCLR", 0xBF886108},
    {"PORTA",    0xBF886010}, {"PORTB",    0xBF886110},
    {"LATA",     0xBF886020}, {"LATASET",  0xBF886024}, {"LATACLR",  0xBF886028}, {"LATATGL", 0xBF88602C},
    {"LATB",     0xBF886120}, {"LATBSET",  0xBF886124}, {"LATBCLR",  0xBF886128}, {"LATBTGL", 0xBF88612C},
    {NULL,       0x0}
};

/* --- FORWARD DECLARATIONS --- */
static void execute_statement(const char *src);
static int evaluate_expression(const char **str);
static void evaluate_string_expr(const char **src, char *dest_buf, size_t max_len);

static void do_print(const char **src);
static void do_let(const char **src);
static void do_goto(const char **src);
static void do_if(const char **src);
static void do_input(const char **src);
static void do_end(const char **src);
static void do_for(const char **src);
static void do_next(const char **src);
static void do_gosub(const char **src);
static void do_return(const char **src);
static void do_dim(const char **src);
static void do_data(const char **src);
static void do_read(const char **src);
static void do_restore(const char **src);
static void do_poke(const char **src);

/* --- MASTER KEYWORD DISPATCH TABLE --- */
static const KeywordEntry KEYWORD_TABLE[] = {
    {">=",      TOKEN_GE,      NULL},
    {"<=",      TOKEN_LE,      NULL},
    {"<>",      TOKEN_NE,      NULL},
    {"!=",      TOKEN_NE,      NULL},
    {"==",      TOKEN_EQ,      NULL},
    {"PRINT",   TOKEN_PRINT,   do_print},   
    {"LET",     TOKEN_LET,     do_let},
    {"GOTO",    TOKEN_GOTO,    do_goto},   
    {"IF",      TOKEN_IF,      do_if},
    {"THEN",    TOKEN_THEN,    NULL},       
    {"INPUT",   TOKEN_INPUT,   do_input},
    {"END",     TOKEN_END,     do_end},     
    {"LIST",    TOKEN_LIST,    NULL},
    {"RUN",     TOKEN_RUN,     NULL},       
    {"CLEAR",   TOKEN_CLEAR,   NULL},
    {"FOR",     TOKEN_FOR,     do_for},     
    {"TO",      TOKEN_TO,      NULL},
    {"NEXT",    TOKEN_NEXT,    do_next},    
    {"STEP",    TOKEN_STEP,    NULL},
    {"GOSUB",   TOKEN_GOSUB,   do_gosub},   
    {"RETURN",  TOKEN_RETURN,  do_return},
    {"DIM",     TOKEN_DIM,     do_dim},     
    {"DATA",    TOKEN_DATA,    do_data},
    {"READ",    TOKEN_READ,    do_read},    
    {"RESTORE", TOKEN_RESTORE, do_restore},
    {"POKE",    TOKEN_POKE,    do_poke},    
    {"PEEK",    TOKEN_PEEK,    NULL},
    {"RND",     TOKEN_RND,     NULL},       
    {"ABS",     TOKEN_ABS,     NULL},
    {"SGN",     TOKEN_SGN,     NULL},       
    {"CLAMP",   TOKEN_CLAMP,   NULL},
    {"BITREAD", TOKEN_BITREAD, NULL},       
    {"BITSET",  TOKEN_BITSET,  NULL},
    {"BITCLR",  TOKEN_BITCLR,  NULL},       
    {"BIT",     TOKEN_BIT,     NULL},
    {"LEN",     TOKEN_LEN,     NULL},       
    {"VAL",     TOKEN_VAL,     NULL},
    {"STR$",    TOKEN_STR,     NULL},       
    {"LEFT$",   TOKEN_LEFT,    NULL},
    {"RIGHT$",  TOKEN_RIGHT,   NULL},       
    {NULL,      0,             NULL},

};

/* --- UTILITY & PARSING HELPERS --- */
static void skip_spaces(const char **str) {
    while (**str == ' ' || **str == '\t') (*str)++;
}

static VarType parse_variable_ident(const char **src, int *var_idx) {
    *var_idx = toupper((unsigned char)**src) - 'A';
    (*src)++;
    if (**src == '$') { (*src)++; return VAR_TYPE_STRING; }
    if (**src == '(') { return VAR_TYPE_ARRAY; }
    return VAR_TYPE_NUMERIC;
}

static int find_sfr_address(const char **str, uint32_t *out_addr) {
    skip_spaces(str);
    for (int i = 0; SFR_TABLE[i].name != NULL; i++) {
        size_t len = strlen(SFR_TABLE[i].name);
        if (strncasecmp(*str, SFR_TABLE[i].name, len) == 0) {
            char next_char = (*str)[len];
            if (!isalnum((unsigned char)next_char) && next_char != '_') {
                *out_addr = SFR_TABLE[i].address;
                *str += len;
                return 1;
            }
        }
    }
    return 0;
}

/* --- FACTOR FUNCTION HANDLERS --- */
static int func_peek(const char **str) {
    if (**str == '(') {
        (*str)++;
        uint32_t addr = (uint32_t)evaluate_expression(str);
        if (**str == ')') (*str)++;
        if (addr & 0x03) { printf("ERR: Unaligned PEEK\n"); return 0; }
        return (int)(*(volatile uint32_t *)addr);
    }
    return 0;
}
static int func_rnd(const char **str) {
    if (**str == '(') {
        (*str)++; int max = evaluate_expression(str); if (**str == ')') (*str)++;
        return (max > 0) ? (rand() % max) : 0;
    }
    return 0;
}
static int func_abs(const char **str) {
    if (**str == '(') {
        (*str)++; int val = evaluate_expression(str); if (**str == ')') (*str)++;
        return (val < 0) ? -val : val;
    }
    return 0;
}
static int func_sgn(const char **str) {
    if (**str == '(') {
        (*str)++; int val = evaluate_expression(str); if (**str == ')') (*str)++;
        return (val > 0) - (val < 0);
    }
    return 0;
}
static int func_clamp(const char **str) {
    if (**str == '(') {
        (*str)++; int val = evaluate_expression(str); if (**str == ',') (*str)++;
        int min = evaluate_expression(str); if (**str == ',') (*str)++;
        int max = evaluate_expression(str); if (**str == ')') (*str)++;
        if (val < min) return min; if (val > max) return max; return val;
    }
    return 0;
}
static int func_bit(const char **str) {
    if (**str == '(') {
        (*str)++; int b = evaluate_expression(str); if (**str == ')') (*str)++;
        return (1 << (b & 0x1F));
    }
    return 0;
}
static int func_bitread(const char **str) {
    if (**str == '(') {
        (*str)++; int val = evaluate_expression(str); if (**str == ',') (*str)++;
        int b = evaluate_expression(str); if (**str == ')') (*str)++;
        return (val >> (b & 0x1F)) & 1;
    }
    return 0;
}
static int func_bitset(const char **str) {
    if (**str == '(') {
        (*str)++; int val = evaluate_expression(str); if (**str == ',') (*str)++;
        int b = evaluate_expression(str); if (**str == ')') (*str)++;
        return val | (1 << (b & 0x1F));
    }
    return 0;
}
static int func_bitclr(const char **str) {
    if (**str == '(') {
        (*str)++; int val = evaluate_expression(str); if (**str == ',') (*str)++;
        int b = evaluate_expression(str); if (**str == ')') (*str)++;
        return val & ~(1 << (b & 0x1F));
    }
    return 0;
}
static int func_len(const char **str) {
    if (**str == '(') {
        (*str)++; skip_spaces(str); int result = 0;
        if (isalpha((unsigned char)**str) && (*str)[1] == '$') {
            int sidx = toupper((unsigned char)**str) - 'A'; *str += 2;
            result = (int)strlen(string_vars[sidx]);
        }
        if (**str == ')') (*str)++;
        return result;
    }
    return 0;
}
static int func_val(const char **str) {
    if (**str == '(') {
        (*str)++; skip_spaces(str); int result = 0;
        if (isalpha((unsigned char)**str) && (*str)[1] == '$') {
            int sidx = toupper((unsigned char)**str) - 'A'; *str += 2;
            result = atoi(string_vars[sidx]);
        }
        if (**str == ')') (*str)++;
        return result;
    }
    return 0;
}

static const FactorFuncEntry FACTOR_FUNC_TABLE[] = {
    {TOKEN_PEEK,    func_peek},    {TOKEN_RND,     func_rnd},
    {TOKEN_ABS,     func_abs},     {TOKEN_SGN,     func_sgn},
    {TOKEN_CLAMP,   func_clamp},   {TOKEN_BIT,     func_bit},
    {TOKEN_BITREAD, func_bitread}, {TOKEN_BITSET,  func_bitset},
    {TOKEN_BITCLR,  func_bitclr},  {TOKEN_LEN,     func_len},
    {TOKEN_VAL,     func_val},     {0,             NULL}
};

/* --- TABLE-DRIVEN PARSER & EXPRESSION EVALUATOR --- */
static int parse_factor(const char **str) {
    skip_spaces(str);
    int result = 0;
    uint8_t token = (uint8_t)**str;

    for (int i = 0; FACTOR_FUNC_TABLE[i].token != 0; i++) {
        if (FACTOR_FUNC_TABLE[i].token == token) {
            (*str)++;
            skip_spaces(str);
            return FACTOR_FUNC_TABLE[i].handler(str);
        }
    }

    if (**str == '(') {
        (*str)++; result = evaluate_expression(str); skip_spaces(str);
        if (**str == ')') (*str)++;
    } else if (isdigit((unsigned char)**str)) {
        while (isdigit((unsigned char)**str)) {
            result = result * 10 + (**str - '0'); (*str)++;
        }
    } else if (**str == '0' && ((*str)[1] == 'x' || (*str)[1] == 'X')) {
        *str += 2; result = (int)strtoul(*str, (char **)str, 16);
    } else {
        uint32_t sfr_addr;
        if (find_sfr_address(str, &sfr_addr)) {
            result = (int)sfr_addr;
        } else if (isalpha((unsigned char)**str)) {
            char var = toupper((unsigned char)**str) - 'A'; (*str)++; skip_spaces(str);
            if (**str == '(') {
                (*str)++; int index = evaluate_expression(str); skip_spaces(str);
                if (**str == ')') (*str)++;
                if (index < 0 || index >= array_vars[(int)var].size) {
                    printf("ERR: Array bounds\n"); result = 0;
                } else {
                    result = array_vars[(int)var].data[index];
                }
            } else {
                result = variables[(int)var];
            }
        }
    }
    return result;
}

static int parse_term(const char **str) {
    skip_spaces(str);
    int result = parse_factor(str);
    skip_spaces(str);
    while (**str == '*' || **str == '/') {
        char op = **str; (*str)++;
        int next_factor = parse_factor(str);
        if (op == '*') result *= next_factor;
        else if (op == '/' && next_factor != 0) result /= next_factor;
        skip_spaces(str);
    }
    return result;
}

static int evaluate_expression(const char **str) {
    skip_spaces(str);
    int result = parse_term(str);
    skip_spaces(str);
    while (**str == '+' || **str == '-') {
        char op = **str; (*str)++;
        int next_term = parse_term(str);
        if (op == '+') result += next_term;
        else if (op == '-') result -= next_term;
        skip_spaces(str);
    }
    return result;
}

static void evaluate_string_expr(const char **src, char *dest_buf, size_t max_len) {
    skip_spaces(src);
    uint8_t token = (uint8_t)**src;

    if (token == TOKEN_STR) {
        (*src)++;
        if (**src == '(') {
            (*src)++; int num = evaluate_expression(src); if (**src == ')') (*src)++;
            snprintf(dest_buf, max_len, "%d", num);
        }
    } else if (token == TOKEN_LEFT || token == TOKEN_RIGHT) {
        (*src)++;
        if (**src == '(') {
            (*src)++; skip_spaces(src);
            if (isalpha((unsigned char)**src) && (*src)[1] == '$') {
                int sidx = toupper((unsigned char)**src) - 'A'; *src += 2; skip_spaces(src);
                if (**src == ',') (*src)++;
                int n = evaluate_expression(src); if (**src == ')') (*src)++;
                int src_len = strlen(string_vars[sidx]);
                if (n > src_len) n = src_len; if (n < 0) n = 0;
                if (token == TOKEN_LEFT) {
                    strncpy(dest_buf, string_vars[sidx], n); dest_buf[n] = '\0';
                } else {
                    strncpy(dest_buf, string_vars[sidx] + (src_len - n), n); dest_buf[n] = '\0';
                }
            }
        }
    } else if (**src == '"') {
        (*src)++; size_t i = 0;
        while (**src && **src != '"' && i < max_len - 1) dest_buf[i++] = *(*src)++;
        dest_buf[i] = '\0'; if (**src == '"') (*src)++;
    } else if (isalpha((unsigned char)**src) && (*src)[1] == '$') {
        int sidx = toupper((unsigned char)**src) - 'A'; *src += 2;
        strncpy(dest_buf, string_vars[sidx], max_len - 1);
        dest_buf[max_len - 1] = '\0';
    }
}

/* --- STATEMENT HANDLERS --- */
static void do_print(const char **src) {
    skip_spaces(src);
    if (**src == '\0') { putchar('\n'); return; }
    if (**src == '"') {
        (*src)++; while (**src && **src != '"') putchar(*(*src)++);
        if (**src == '"') (*src)++; putchar('\n');
    } else if (isalpha((unsigned char)**src) && (*src)[1] == '$') {
        int var_idx = toupper((unsigned char)**src) - 'A'; *src += 2;
        printf("%s\n", string_vars[var_idx]);
    } else {
        printf("%d\n", evaluate_expression(src));
    }
}

static void do_let(const char **src) {
    skip_spaces(src);
    if (!isalpha((unsigned char)**src)) return;
    int var_idx;
    VarType type = parse_variable_ident(src, &var_idx);
    skip_spaces(src);
    if (**src != '=') return;
    (*src)++; skip_spaces(src);

    if (type == VAR_TYPE_STRING) {
        evaluate_string_expr(src, string_vars[var_idx], MAX_STRING_LEN);
    } else if (type == VAR_TYPE_ARRAY) {
        (*src)++; int index = evaluate_expression(src);
        if (**src == ')') (*src)++; skip_spaces(src);
        if (**src == '=') {
            (*src)++;
            if (index >= 0 && index < array_vars[var_idx].size) {
                array_vars[var_idx].data[index] = evaluate_expression(src);
            }
        }
    } else {
        variables[var_idx] = evaluate_expression(src);
    }
}

static int find_line_index(uint16_t line_num) {
    for (int i = 0; i < line_count; i++) {
        if (line_index_table[i].line_number == line_num) return i;
    }
    return -1;
}

static const char* get_line_text(int table_idx) {
    if (table_idx < 0 || table_idx >= line_count) return NULL;
    return &program_pool[line_index_table[table_idx].offset];
}

static void do_goto(const char **src) {
    int target_line = evaluate_expression(src);
    int target_idx = find_line_index(target_line);
    if (target_idx != -1) current_exec_index = target_idx;
    else { printf("ERR: Line %d not found\n", target_line); current_exec_index = -1; }
}

static void do_if(const char **src) {
    int val1 = evaluate_expression(src);
    skip_spaces(src);

    /* Fetch the operator byte (whether a token >0x80 or ASCII <0x80) */
    uint8_t op = (uint8_t)*(*src)++;

    int val2 = evaluate_expression(src);
    int condition = 0;

    switch (op) {
        case '=':
        case TOKEN_EQ: condition = (val1 == val2); break;
        case '>':      condition = (val1 > val2);  break;
        case '<':      condition = (val1 < val2);  break;
        case TOKEN_GE: condition = (val1 >= val2); break;
        case TOKEN_LE: condition = (val1 <= val2); break;
        case TOKEN_NE: condition = (val1 != val2); break;
        default:
            printf("ERR: Syntax in IF operator\n");
            return;
    }

    skip_spaces(src);
    if ((uint8_t)**src == TOKEN_THEN) {
        (*src)++;
        if (condition) execute_statement(*src);
    }
}

static void do_input(const char **src) {
    skip_spaces(src);
    if (!isalpha((unsigned char)**src)) return;
    int var_idx; VarType type = parse_variable_ident(src, &var_idx);
    printf("? "); char buf[MAX_STRING_LEN];
    if (fgets(buf, sizeof(buf), stdin)) {
        buf[strcspn(buf, "\r\n")] = 0;
        if (type == VAR_TYPE_STRING) {
            strncpy(string_vars[var_idx], buf, MAX_STRING_LEN - 1);
            string_vars[var_idx][MAX_STRING_LEN - 1] = '\0';
        } else {
            variables[var_idx] = atoi(buf);
        }
    }
}

static void do_end(const char **src) { (void)src; current_exec_index = -1; }

static void do_for(const char **src) {
    skip_spaces(src);
    if (!isalpha((unsigned char)**src)) return;
    int var = toupper((unsigned char)**src) - 'A'; (*src)++; skip_spaces(src);
    if (**src != '=') return; (*src)++;
    variables[var] = evaluate_expression(src); skip_spaces(src);
    if ((uint8_t)**src != TOKEN_TO) return; (*src)++;
    int target = evaluate_expression(src); int step = 1; skip_spaces(src);
    if ((uint8_t)**src == TOKEN_STEP) { (*src)++; step = evaluate_expression(src); }
    if (for_sp >= MAX_FOR_DEPTH) { printf("ERR: FOR overflow\n"); current_exec_index = -1; return; }

    for_stack[for_sp].var_idx = var; for_stack[for_sp].target_val = target;
    for_stack[for_sp].step_val = step; for_stack[for_sp].line_index = current_exec_index + 1;
    for_sp++;
}

static void do_next(const char **src) {
    skip_spaces(src); int var = -1;
    if (isalpha((unsigned char)**src)) { var = toupper((unsigned char)**src) - 'A'; (*src)++; }
    if (for_sp <= 0) { printf("ERR: NEXT without FOR\n"); current_exec_index = -1; return; }

    int frame_idx = for_sp - 1;
    if (var != -1 && for_stack[frame_idx].var_idx != var) {
        while (frame_idx >= 0 && for_stack[frame_idx].var_idx != var) frame_idx--;
        if (frame_idx < 0) { printf("ERR: NEXT mismatch\n"); current_exec_index = -1; return; }
    }
    ForLoopFrame *frame = &for_stack[frame_idx];
    variables[frame->var_idx] += frame->step_val;
    int done = (frame->step_val >= 0) ? (variables[frame->var_idx] > frame->target_val)
                                      : (variables[frame->var_idx] < frame->target_val);
    if (!done) current_exec_index = frame->line_index;
    else for_sp = frame_idx;
}

static void do_gosub(const char **src) {
    int target_line = evaluate_expression(src);
    int target_idx = find_line_index(target_line);
    if (target_idx == -1 || gosub_sp >= MAX_GOSUB_DEPTH) { current_exec_index = -1; return; }
    gosub_stack[gosub_sp++] = current_exec_index + 1;
    current_exec_index = target_idx;
}

static void do_return(const char **src) {
    (void)src;
    if (gosub_sp <= 0) { current_exec_index = -1; return; }
    current_exec_index = gosub_stack[--gosub_sp];
}

static void do_dim(const char **src) {
    skip_spaces(src); if (!isalpha((unsigned char)**src)) return;
    int arr_idx = toupper((unsigned char)**src) - 'A'; (*src)++; skip_spaces(src);
    if (**src != '(') return; (*src)++;
    int size = evaluate_expression(src); if (**src == ')') (*src)++;
    if (size > 0 && size <= MAX_ARRAY_SIZE) array_vars[arr_idx].size = size;
}

static void do_data(const char **src) { (void)src; }

static void do_read(const char **src) {
    skip_spaces(src); if (!isalpha((unsigned char)**src)) return;
    int var_idx; VarType type = parse_variable_ident(src, &var_idx);
    int array_idx = 0;
    if (type == VAR_TYPE_ARRAY && **src == '(') {
        (*src)++; array_idx = evaluate_expression(src); if (**src == ')') (*src)++;
    }
    while (data_line_idx < line_count) {
        const char *line_ptr = get_line_text(data_line_idx) + data_char_offset;
        skip_spaces(&line_ptr);
        if (data_char_offset == 0) {
            if ((uint8_t)*line_ptr == TOKEN_DATA) line_ptr++;
            else { data_line_idx++; data_char_offset = 0; continue; }
        }
        skip_spaces(&line_ptr);
        if (*line_ptr != '\0') {
            int val = evaluate_expression(&line_ptr);
            if (type == VAR_TYPE_ARRAY && array_idx >= 0 && array_idx < array_vars[var_idx].size) {
                array_vars[var_idx].data[array_idx] = val;
            } else if (type == VAR_TYPE_NUMERIC) {
                variables[var_idx] = val;
            }
            skip_spaces(&line_ptr);
            if (*line_ptr == ',') {
                line_ptr++; data_char_offset = (int)(line_ptr - get_line_text(data_line_idx));
            } else { data_line_idx++; data_char_offset = 0; }
            return;
        }
        data_line_idx++; data_char_offset = 0;
    }
    printf("ERR: Out of DATA\n"); current_exec_index = -1;
}

static void do_restore(const char **src) { (void)src; data_line_idx = 0; data_char_offset = 0; }

static void do_poke(const char **src) {
    uint32_t addr = (uint32_t)evaluate_expression(src); skip_spaces(src);
    if (**src == ',') (*src)++;
    uint32_t value = (uint32_t)evaluate_expression(src);
    if (addr & 0x03) { printf("ERR: Unaligned POKE\n"); return; }
    *(volatile uint32_t *)addr = value;
}

/* --- TOKENIZER & ARENA MEMORY COMPACTION --- */
static void tokenize_string(const char *in, char *out) {
    while (*in) {
        if (*in == '"') {
            *out++ = *in++; while (*in && *in != '"') *out++ = *in++;
            if (*in) *out++ = *in++; continue;
        }
        int matched = 0;
        for (int i = 0; KEYWORD_TABLE[i].keyword != NULL; i++) {
            size_t len = strlen(KEYWORD_TABLE[i].keyword);
            if (strncasecmp(in, KEYWORD_TABLE[i].keyword, len) == 0) {
                *out++ = (char)KEYWORD_TABLE[i].token; in += len; matched = 1; break;
            }
        }
        if (!matched) *out++ = *in++;
    }
    *out = '\0';
}

static void print_detokenized(const char *src) {
    while (*src) {
        uint8_t ch = (uint8_t)*src;
        if (ch >= 0x80) {
            for (int i = 0; KEYWORD_TABLE[i].keyword != NULL; i++) {
                if (KEYWORD_TABLE[i].token == ch) { printf("%s", KEYWORD_TABLE[i].keyword); break; }
            }
        } else putchar(ch);
        src++;
    }
    putchar('\n');
}

static void compact_pool(uint16_t hole_offset, uint16_t hole_length) {
    if (hole_length == 0) return;
    uint16_t bytes_to_move = pool_bytes_used - (hole_offset + hole_length);
    memmove(&program_pool[hole_offset], &program_pool[hole_offset + hole_length], bytes_to_move);
    pool_bytes_used -= hole_length;
    for (int i = 0; i < line_count; i++) {
        if (line_index_table[i].offset > hole_offset) line_index_table[i].offset -= hole_length;
    }
}

static void store_line(uint16_t line_num, const char *text) {
    char tokenized_text[MAX_LINE_LEN];
    tokenize_string(text, tokenized_text);
    int idx = find_line_index(line_num);
    uint8_t new_len = (uint8_t)(strlen(tokenized_text) + 1);

    if (tokenized_text[0] == '\0') {
        if (idx != -1) {
            compact_pool(line_index_table[idx].offset, line_index_table[idx].length);
            for (int i = idx; i < line_count - 1; i++) line_index_table[i] = line_index_table[i + 1];
            line_count--;
        }
        return;
    }
    if (idx != -1) compact_pool(line_index_table[idx].offset, line_index_table[idx].length);
    if (pool_bytes_used + new_len > POOL_SIZE) { printf("ERR: Memory pool full\n"); return; }

    int insert_idx = idx;
    if (insert_idx == -1) {
        insert_idx = line_count - 1;
        while (insert_idx >= 0 && line_index_table[insert_idx].line_number > line_num) {
            line_index_table[insert_idx + 1] = line_index_table[insert_idx]; insert_idx--;
        }
        insert_idx++; line_count++;
    }
    uint16_t new_offset = pool_bytes_used;
    memcpy(&program_pool[new_offset], tokenized_text, new_len);
    pool_bytes_used += new_len;
    line_index_table[insert_idx].line_number = line_num;
    line_index_table[insert_idx].offset = new_offset;
    line_index_table[insert_idx].length = new_len;
}

static void execute_statement(const char *src) {
    skip_spaces(&src); if (*src == '\0') return;
    uint8_t token = (uint8_t)*src; src++;
    for (int i = 0; KEYWORD_TABLE[i].keyword != NULL; i++) {
        if (KEYWORD_TABLE[i].token == token && KEYWORD_TABLE[i].handler != NULL) {
            KEYWORD_TABLE[i].handler(&src); return;
        }
    }
}

static void run_program(void) {
    gosub_sp = 0; for_sp = 0; data_line_idx = 0; data_char_offset = 0;
    current_exec_index = 0;
    while (current_exec_index >= 0 && current_exec_index < line_count) {
        int prev_index = current_exec_index;
        execute_statement(get_line_text(current_exec_index));
        if (current_exec_index == prev_index) current_exec_index++;
    }
}

/* --- MAIN ENTRY POINT --- */
int main(void) {
    char input_buffer[MAX_LINE_LEN];
    printf("\n--- PIC32 Tokenized BASIC ---\nReady\n");

    while (1) {
        printf("> ");
        if (!fgets(input_buffer, sizeof(input_buffer), stdin)) break;
        input_buffer[strcspn(input_buffer, "\r\n")] = 0;
        const char *ptr = input_buffer; skip_spaces(&ptr);

        if (isdigit((unsigned char)*ptr)) {
            int line_num = atoi(ptr);
            while (isdigit((unsigned char)*ptr)) ptr++;
            skip_spaces(&ptr); store_line((uint16_t)line_num, ptr);
        } else if (strcasecmp(ptr, "RUN") == 0) {
            run_program();
        } else if (strcasecmp(ptr, "LIST") == 0) {
            for (int i = 0; i < line_count; i++) {
                printf("%d ", line_index_table[i].line_number);
                print_detokenized(get_line_text(i));
            }
        } else if (strcasecmp(ptr, "CLEAR") == 0) {
            line_count = 0; pool_bytes_used = 0;
            memset(variables, 0, sizeof(variables));
            memset(string_vars, 0, sizeof(string_vars));
            memset(array_vars, 0, sizeof(array_vars));
        } else {
            char tokenized[MAX_LINE_LEN];
            tokenize_string(ptr, tokenized);
            execute_statement(tokenized);
        }
    }
    return 0;
}