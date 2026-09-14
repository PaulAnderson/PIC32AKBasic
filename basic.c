#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

/* --- CONFIGURATION & MEMORY LIMITS --- */
#define INTERPRETER_BASE_ADDR ((uintptr_t)program_pool)
#define INTERPRETER_END_ADDR ((uintptr_t)end_interpreter_ram)
#define INTERPRETER_RAM_SIZE  (INTERPRETER_END_ADDR - INTERPRETER_BASE_ADDR)

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
    TOKEN_MEM,
    TOKEN_DUMP,
    TOKEN_LINEPTR,
    TOKEN_VARPTR,
    TOKEN_HEX,
    TOKEN_REM,
    TOKEN_CHR
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

static char end_interpreter_ram[1];

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
static void execute_statement_ptr(const char **src);
static void execute_statement(const char **src);
static void execute_line_buffer(const char *buf);
static int evaluate_expression(const char **str);
static void evaluate_string_expr(const char **src, char *dest_buf, size_t max_len);
static int find_line_index(uint16_t line_num);
static const char* get_line_text(int table_idx);

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
static void do_mem_stat(const char **src);
static void do_dump(const char **src);
static void tokenize_string(const char *in, char *out);


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
    {"MEM", TOKEN_MEM, do_mem_stat},
    {"DUMP", TOKEN_DUMP, do_dump},
    {"LINEPTR", TOKEN_LINEPTR, NULL},
    {"VARPTR", TOKEN_VARPTR, NULL},
    {"HEX$", TOKEN_HEX, NULL},
    {"REM", TOKEN_REM, NULL},
    {"CHR$", TOKEN_CHR, NULL},
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
    skip_spaces(src);
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
        uintptr_t addr = (uintptr_t)evaluate_expression(str);
        if (**str == ')') (*str)++;

        if (addr < INTERPRETER_RAM_SIZE) {
            uintptr_t actual_addr = INTERPRETER_BASE_ADDR + addr;
            return (uint8_t)(*(volatile uint8_t *)actual_addr);
        }

        /* 2. ABSOLUTE PHYSICAL ACCESS: SFRs and Hardware RAM */
#if defined(__XC32__) || defined(__PIC32MX__) || defined(__PIC32MZ__) || defined(__PIC32AK__)
        if (addr & 0x03) {
            printf("ERR: Unaligned 32-bit PEEK\n");
            return 0;
        }
        /* Read 32-bit SFR or peripheral register */
        return (int)(*(volatile uint32_t *)addr);
#else
        /* Host GCC Guard (WSL/Linux) */
        printf("ERR: Absolute physical PEEK blocked on host OS\n");
        printf("Valid range: 0 - %lu\n", INTERPRETER_RAM_SIZE);
        return 0;
#endif
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

static int func_mem(const char **str) {
    int mode = 0; /* Default: 0 = Free Memory */

    if (**str == '(') {
        (*str)++;
        skip_spaces(str);
        if (**str != ')') {
            mode = evaluate_expression(str);
        }
        if (**str == ')') (*str)++;
    }

    if (mode == 1) {
        /* Return used bytes (pool bytes + index table overhead) */
        return (int)(pool_bytes_used + (line_count * sizeof(LineIndex)));
    }
    
    /* Default (mode == 0): Return remaining free bytes */
    int total_capacity = POOL_SIZE;
    int current_used = pool_bytes_used + (line_count * sizeof(LineIndex));
    int remaining = total_capacity - current_used;

    return (remaining < 0) ? 0 : remaining;
}

static int func_lineptr(const char **str) {
    if (**str == '(') {
        (*str)++;
        int target_line = evaluate_expression(str);
        if (**str == ')') (*str)++;

        int idx = find_line_index(target_line);
        if (idx == -1) {
            printf("ERR: Line %d not found\n", target_line);
            return -1;
        }

        /* Calculate address relative to the interpreter's base memory space */
        uintptr_t absolute_addr = (uintptr_t)get_line_text(idx);
        return (int)(absolute_addr - INTERPRETER_BASE_ADDR);
    }
    return -1;
}
static int func_varptr(const char **str) {
    if (**str == '(') {
        (*str)++;
        skip_spaces(str);

        if (!isalpha((unsigned char)**str)) {
            printf("ERR: VARPTR expects variable\n");
            return -1;
        }

        int var_idx;
        VarType type = parse_variable_ident(str, &var_idx);
        uintptr_t target_addr = 0;

        if (type == VAR_TYPE_STRING) {
            /* Address of string buffer (e.g. basic_state.string_vars[var_idx]) */
            target_addr = (uintptr_t)&string_vars[var_idx][0];
        } 
        else if (type == VAR_TYPE_ARRAY) {
            /* Expects index: VARPTR(A(0)) */
            if (**str == '(') {
                (*str)++;
                int elem_idx = evaluate_expression(str);
                if (**str == ')') (*str)++;

                if (elem_idx >= 0 && elem_idx < array_vars[var_idx].size) {
                    target_addr = (uintptr_t)&array_vars[var_idx].data[elem_idx];
                } else {
                    printf("ERR: Array index out of bounds\n");
                    return -1;
                }
            } else {
                /* Defaults to start of array data if no index specified */
                target_addr = (uintptr_t)&array_vars[var_idx].data[0];
            }
        } 
        else {
            /* Address of standard numeric variable A-Z */
            target_addr = (uintptr_t)&variables[var_idx];
        }

        skip_spaces(str);
        if (**str == ')') (*str)++;

        /* Return offset relative to INTERPRETER_BASE_ADDR */
        return (int)(target_addr - INTERPRETER_BASE_ADDR);
    }

    printf("ERR: VARPTR syntax error\n");
    return -1;
}

static void func_hex_str(const char **str, char *out_str, size_t max_len) {
    if (**str == '(') {
        (*str)++;
        uint32_t val = (uint32_t)evaluate_expression(str);
        if (**str == ')') (*str)++;

        char temp[16];
        /* Format to full 8-digit uppercase hex first */
        snprintf(temp, sizeof(temp), "%X", (unsigned int)val);

        //trim leading 0s
        const char *p = temp;
        while (*p == '0' && *(p + 1) != '\0') {
            p++;
        }

        /* Copy trimmed result to output buffer */
        strncpy(out_str, p, max_len - 1);
        out_str[max_len - 1] = '\0';
        return;
    }

    out_str[0] = '\0';
    printf("ERR: HEX$ expects (expr)\n");
}

static void func_chr_str(const char **str, char *out_str, size_t max_len) {
    if (max_len < 2) return;

    if (**str == '(') {
        (*str)++;
        int code = evaluate_expression(str);
        if (**str == ')') (*str)++;

        /* Convert integer ASCII code to a 1-character null-terminated string */
        out_str[0] = (char)(code & 0xFF);
        out_str[1] = '\0';
        return;
    }

    out_str[0] = '\0';
    printf("ERR: CHR$ expects (expr)\n");
}

static const FactorFuncEntry FACTOR_FUNC_TABLE[] = {
    {TOKEN_PEEK,    func_peek},    
    {TOKEN_RND,     func_rnd},
    {TOKEN_ABS,     func_abs},     
    {TOKEN_SGN,     func_sgn},
    {TOKEN_CLAMP,   func_clamp},   
    {TOKEN_BIT,     func_bit},
    {TOKEN_BITREAD, func_bitread}, 
    {TOKEN_BITSET,  func_bitset},
    {TOKEN_BITCLR,  func_bitclr},  
    {TOKEN_LEN,     func_len},
    {TOKEN_VAL,     func_val},     
    {TOKEN_MEM, func_mem},
    {TOKEN_LINEPTR, func_lineptr},
    {TOKEN_VARPTR, func_varptr},
    {0,             NULL}
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
    int result = parse_factor(str);
    skip_spaces(str);

    while (**str == '*' || **str == '/' || **str == '%') {
        char op = *(*str)++;
        int next_factor = parse_factor(str);

        if (op == '*') {
            result *= next_factor;
        } 
        else if (op == '/') {
            if (next_factor == 0) {
                printf("ERR: Division by zero\n");
                return 0;
            }
            result /= next_factor;
        } 
        else if (op == '%') {
            if (next_factor == 0) {
                printf("ERR: Modulo by zero\n");
                return 0;
            }
            result %= next_factor;
        }

        skip_spaces(str);
    }

    return result;
}

static int evaluate_expression(const char **str) {
    skip_spaces(str);
    int result = parse_term(str);
    skip_spaces(str);

    /* 1. Additive operators (+, -) */
    while (**str == '+' || **str == '-') {
        char op = **str; (*str)++;
        int next_term = parse_term(str);
        if (op == '+') result += next_term;
        else if (op == '-') result -= next_term;
        skip_spaces(str);
    }

    /* 2. Relational operators (=, <, >, <=, >=, <>) */
    uint8_t tok = (uint8_t)**str;
    
    if (tok == '=' || tok == TOKEN_EQ) {
        (*str)++;
        return (result == evaluate_expression(str));
    }
    else if (tok == '<') {
        (*str)++;
        if (**str == '>') { (*str)++; return (result != evaluate_expression(str)); }
        if (**str == '=') { (*str)++; return (result <= evaluate_expression(str)); }
        return (result < evaluate_expression(str));
    }
    else if (tok == '>') {
        (*str)++;
        if (**str == '=') { (*str)++; return (result >= evaluate_expression(str)); }
        return (result > evaluate_expression(str));
    }
    else if (tok == TOKEN_GE) { (*str)++; return (result >= evaluate_expression(str)); }
    else if (tok == TOKEN_LE) { (*str)++; return (result <= evaluate_expression(str)); }
    else if (tok == TOKEN_NE) { (*str)++; return (result != evaluate_expression(str)); }

    return result;
}

static void evaluate_string_expr(const char **src, char *dest_buf, size_t max_len) {
    skip_spaces(src);
    uint8_t token = (uint8_t)**src;

    if (token == TOKEN_HEX) {
        (*src)++;  
        func_hex_str(src, dest_buf, max_len);
    } else if (token == TOKEN_CHR) {
        (*src)++;  
        func_chr_str(src, dest_buf, max_len);
    } else if (token == TOKEN_STR) {
        (*src)++;
        if (**src == '(') {
            (*src)++; 
            int num = evaluate_expression(src); 
            if (**src == ')') (*src)++;
            snprintf(dest_buf, max_len, "%d", num);
        }
    } else if (token == TOKEN_LEFT || token == TOKEN_RIGHT) {
        (*src)++;
        if (**src == '(') {
            (*src)++; skip_spaces(src);
            if (isalpha((unsigned char)**src) && (*src)[1] == '$') {
                int sidx = toupper((unsigned char)**src) - 'A'; *src += 2; skip_spaces(src);
                if (**src == ',') (*src)++;
                int n = evaluate_expression(src); 
                if (**src == ')') (*src)++;
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
        dest_buf[i] = '\0'; 
        if (**src == '"') (*src)++;
    } else if (isalpha((unsigned char)**src) && (*src)[1] == '$') {
        int sidx = toupper((unsigned char)**src) - 'A'; *src += 2;
        strncpy(dest_buf, string_vars[sidx], max_len - 1);
        dest_buf[max_len - 1] = '\0';
    }

    skip_spaces(src);
}

/* --- STATEMENT HANDLERS --- */
static void do_print(const char **src) {
    skip_spaces(src);
    if (**src == '\0' || **src == ':') {
        printf("\n");
        return;
    }

    int trailing_delimiter = 0;

    while (**src != '\0' && **src != ':') {
        skip_spaces(src);
        if (**src == '\0' || **src == ':') break;

        uint8_t token = (uint8_t)**src;

        /* STOP PRINTING if we encounter another command token (>= 0x80) */
        if (token >= 0x80 && token != TOKEN_HEX && token != TOKEN_CHR && 
            token != TOKEN_STR && token != TOKEN_LEFT && token != TOKEN_RIGHT) {
            break;
        }

        if (token == TOKEN_HEX || token == TOKEN_CHR || token == TOKEN_STR || 
            token == TOKEN_LEFT || token == TOKEN_RIGHT || **src == '"' || 
            (((*src)[1] == '$') && isalpha((unsigned char)**src))) {
            
            char str_buf[MAX_STRING_LEN] = {0};
            evaluate_string_expr(src, str_buf, sizeof(str_buf));
            printf("%s", str_buf);
            trailing_delimiter = 0;
        } else {
            int val = evaluate_expression(src);
            printf("%d", val);
            trailing_delimiter = 0;
        }

        skip_spaces(src);

        if (**src == ';') {
            (*src)++;
            trailing_delimiter = 1;
        } else if (**src == ',') {
            (*src)++;
            printf("\t");
            trailing_delimiter = 1;
        } else {
            break;
        }
    }

    if (!trailing_delimiter) {
        printf("\n");
    }
}
static void do_let(const char **src) {
    skip_spaces(src);
    if (!isalpha((unsigned char)**src)) return;

    int var_idx;
    VarType type = parse_variable_ident(src, &var_idx);
    skip_spaces(src);

    if (type == VAR_TYPE_STRING) {
        if (**src == '=') (*src)++;
        evaluate_string_expr(src, string_vars[var_idx], MAX_STRING_LEN);
    } 
    else if (type == VAR_TYPE_ARRAY) {
        if (**src != '(') return;
        (*src)++; /* Skip '(' */

        int index = evaluate_expression(src);
        skip_spaces(src);

        if (**src == ')') (*src)++;
        skip_spaces(src);

        if (**src == '=') {
            (*src)++;
            if (index >= 0 && index < array_vars[var_idx].size) {
                array_vars[var_idx].data[index] = evaluate_expression(src);
            } else {
                printf("ERR: Array index out of bounds (%d)\n", index);
            }
        }
    } 
    else {
        if (**src == '=') (*src)++;
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

static const char *find_token_caseless(const char *haystack, const char *needle) {
    size_t nlen = strlen(needle);
    if (nlen == 0) return haystack;
    while (*haystack != '\0') {
        if (strncasecmp(haystack, needle, nlen) == 0) {
            return haystack;
        }
        haystack++;
    }
    return NULL;
}


static void do_input(const char **src) {
    skip_spaces(src);

    /* 1. Optional Prompt String Parsing: INPUT "PROMPT: ", VAR */
    if (**src == '"') {
        (*src)++;
        while (**src && **src != '"') {
            putchar(*(*src)++);
        }
        if (**src == '"') (*src)++;
        
        skip_spaces(src);
        if (**src == ';' || **src == ',') (*src)++;
        skip_spaces(src);
    } else {
        printf("? ");
    }

    /* 2. Variable Parsing */
    if (!isalpha((unsigned char)**src)) {
        printf("ERR: INPUT expects variable\n");
        return;
    }

    int var_idx;
    VarType type = parse_variable_ident(src, &var_idx);

    /* 3. Terminal IO Read */
    char buf[MAX_STRING_LEN];
    if (fgets(buf, sizeof(buf), stdin)) {
        buf[strcspn(buf, "\r\n")] = 0; /* Strip newline */

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
    skip_spaces(src);
    if (!isalpha((unsigned char)**src)) return;

    int arr_idx;
    VarType type = parse_variable_ident(src, &arr_idx);

    if (type != VAR_TYPE_ARRAY || **src != '(') {
        printf("ERR: DIM syntax error\n");
        return;
    }
    (*src)++; /* Skip '(' */

    int size = evaluate_expression(src);
    skip_spaces(src);
    if (**src == ')') (*src)++;

    if (size > 0 && size <= MAX_ARRAY_SIZE) {
        array_vars[arr_idx].size = size;
    } else {
        printf("ERR: Invalid array size\n");
    }
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
    uintptr_t addr = (uintptr_t)evaluate_expression(src);
    skip_spaces(src);
    if (**src == ',') (*src)++;
    uint32_t val = (uint32_t)evaluate_expression(src);

    /* 1. RELATIVE ACCESS: Modifies program memory */
    if (addr < INTERPRETER_RAM_SIZE) {
        uintptr_t actual_addr = INTERPRETER_BASE_ADDR + addr;
        *(volatile uint8_t *)actual_addr = (uint8_t)val;
        return;
    }

    /* 2. ABSOLUTE PHYSICAL ACCESS: Writes to hardware SFRs */
#if defined(__XC32__) || defined(__PIC32MX__) || defined(__PIC32MZ__) || defined(__PIC32AK__)
    if (addr & 0x03) {
        printf("ERR: Unaligned 32-bit POKE\n");
        return;
    }
    *(volatile uint32_t *)addr = val;
#else
    /* Host GCC Guard (WSL/Linux) */
    printf("ERR: Absolute physical POKE blocked on host OS\n");
    printf("Valid relative range: 0 - %lu\n", (unsigned long)(INTERPRETER_RAM_SIZE - 1));
#endif
}

static void do_mem_stat(const char **src) {
    (void)src;
    int index_bytes = line_count * sizeof(LineIndex);
    int total_used = pool_bytes_used + index_bytes;
    int free_bytes = POOL_SIZE - total_used;

    printf("\n--- Memory Status ---\n");
    printf("Pool Total : %d bytes\n", POOL_SIZE);
    printf("Code Stored: %d bytes\n", pool_bytes_used);
    printf("Index Table: %d bytes (%d lines)\n", index_bytes, line_count);
    printf("Free Memory: %d bytes\n\n", free_bytes < 0 ? 0 : free_bytes);
}

static void dump_single_line(int table_idx) {
    if (table_idx < 0 || table_idx >= line_count) return;

    uint16_t line_num = line_index_table[table_idx].line_number;
    const char *line_text = get_line_text(table_idx);
    uint8_t line_len = line_index_table[table_idx].length;

    printf("%d: ", line_num);

    /* Iterate through every byte including trailing '\0' */
    for (int i = 0; i < line_len; i++) {
        uint8_t ch = (uint8_t)line_text[i];

        if (ch >= 0x80) {
            /* Token Byte: Look up keyword string name */
            const char *kw_name = "UNKNOWN";
            for (int k = 0; KEYWORD_TABLE[k].keyword != NULL; k++) {
                if (KEYWORD_TABLE[k].token == ch) {
                    kw_name = KEYWORD_TABLE[k].keyword;
                    break;
                }
            }
            printf("0x%02X(%s) ", ch, kw_name);
        } 
        else if (ch == '\0') {
            printf("0x00(\\0) ");
        } 
        else if (isprint(ch)) {
            /* Printable ASCII Character */
            printf("%c ", ch);
        } 
        else {
            /* Non-printable byte */
            printf("0x%02X ", ch);
        }
    }

    printf("[%d bytes]\n", line_len);
}

static void do_dump(const char **src) {
    skip_spaces(src);

    if (**src == '\0') {
        //No line specified, dump all lines
        if (line_count == 0) {
            printf("No program in memory\n");
            return;
        }
        for (int i = 0; i < line_count; i++) {
            dump_single_line(i);
        }
        return;
    } else {
        int line_num = evaluate_expression(src);
        int idx = find_line_index(line_num);

        if (idx == -1) {
            printf("ERR: Line %d not found\n", line_num);
            return;
        }

        dump_single_line(idx);
    }
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

static void execute_statement_ptr(const char **src) {
    skip_spaces(src);
    if (**src == '\0' || **src == ':') return;

    uint8_t token = (uint8_t)**src;

    /* Keyword Search */
    for (int i = 0; KEYWORD_TABLE[i].keyword != NULL; i++) {
        if (KEYWORD_TABLE[i].token == token && KEYWORD_TABLE[i].handler != NULL) {
            (*src)++; /* Consume statement token byte */
            KEYWORD_TABLE[i].handler(src); /* Pass double-pointer to handler */
            return;
        }
    }

    /* Implicit LET */
    if (isalpha((unsigned char)**src)) {
        const char *ptr = *src + 1;
        if (*ptr == '$') ptr++;
        skip_spaces(&ptr);

        if (*ptr == '=' || *ptr == '(') {
            do_let(src);
            return;
        }
    }

    printf("ERR: Unknown statement\n");
    *src += strlen(*src); /* Consume line on error */
}

static void execute_statement(const char **src) {
    execute_statement_ptr(src);
}

static void execute_line_buffer(const char *buf) {
    char tokenized[MAX_LINE_LEN];
    tokenize_string(buf, tokenized);
    const char *tok_ptr = tokenized;
    execute_statement_ptr(&tok_ptr);
}

 

static void do_if(const char **src) {
    /* 1. Evaluate condition */
    int condition = evaluate_expression(src);
    skip_spaces(src);

    /* 2. Consume THEN token or keyword */
    int has_then = 0;
    if ((uint8_t)**src == TOKEN_THEN) {
        (*src)++;
        has_then = 1;
    } else if (strncasecmp(*src, "THEN", 4) == 0) {
        *src += 4;
        has_then = 1;
    }

    if (!has_then) {
        printf("ERR: IF without THEN\n");
        *src += strlen(*src);
        return;
    }

    skip_spaces(src);

    /* 3. Locate ELSE branch if present */
    const char *else_ptr = find_token_caseless(*src, "ELSE");

    if (condition) {
        if (else_ptr != NULL) {
            size_t len = else_ptr - *src;
            char then_buf[MAX_LINE_LEN] = {0};
            if (len >= sizeof(then_buf)) len = sizeof(then_buf) - 1;
            strncpy(then_buf, *src, len);
            
            execute_line_buffer(then_buf);
            *src = else_ptr + strlen(else_ptr);
        } else {
            execute_statement_ptr(src);
        }
    } else {
        if (else_ptr != NULL) {
            else_ptr += 4;
            skip_spaces(&else_ptr);
            execute_line_buffer(else_ptr);
            *src += strlen(*src);
        } else {
            /* Condition FALSE: Advance src past this statement up to next ':' or end of line */
            while (**src != '\0' && **src != ':') {
                (*src)++;
            }
        }
    }
}
/*
static void do_if(const char **src) {
    int val1 = evaluate_expression(src);
    skip_spaces(src);

    // Fetch the operator byte (whether a token >0x80 or ASCII <0x80)
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
*/

static void run_program(void) {
    gosub_sp = 0; 
    for_sp = 0; 
    data_line_idx = 0; 
    data_char_offset = 0;
    current_exec_index = 0;

    while (current_exec_index >= 0 && current_exec_index < line_count) {
        int prev_index = current_exec_index;
        const char *line_ptr = get_line_text(current_exec_index);

        while (*line_ptr != '\0' && current_exec_index == prev_index) {
            skip_spaces(&line_ptr);
            if (*line_ptr == '\0') break;

            /* Advance line_ptr in-place across executed statements */
            execute_statement_ptr(&line_ptr);

            skip_spaces(&line_ptr);
            if (*line_ptr == ':') {
                line_ptr++; /* Skip multi-statement separator */
            }
        }

        /* Advance to next line if GOTO/GOSUB didn't redirect execution */
        if (current_exec_index == prev_index) {
            current_exec_index++;
        }
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

            const char *line_ptr = tokenized;
            execute_statement(&line_ptr);
        }
    }
    return 0;
}