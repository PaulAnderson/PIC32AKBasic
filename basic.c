#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

#define MAX_LINES 100
#define MAX_LINE_LEN 80

/* Token Definition Byte Offsets (0x80 - 0x8F) */
enum Token {
    TOKEN_PRINT = 0x80,
    TOKEN_LET,
    TOKEN_GOTO,
    TOKEN_IF,
    TOKEN_THEN,
    TOKEN_INPUT,
    TOKEN_END,
    TOKEN_LIST,
    TOKEN_RUN,
    TOKEN_CLEAR
};

typedef struct {
    const char *keyword;
    uint8_t token;
    void (*handler)(const char **src);
} KeywordEntry;

typedef struct {
    int line_number;
    char text[MAX_LINE_LEN]; /* Stores tokenized program lines */
} ProgramLine;

static ProgramLine program[MAX_LINES];
static int line_count = 0;
static int variables[26];
static int current_exec_index = -1;

/* Forward Declarations for Table Handlers */
static void do_print(const char **src);
static void do_let(const char **src);
static void do_goto(const char **src);
static void do_if(const char **src);
static void do_input(const char **src);
static void do_end(const char **src);

/* Command Table (Alphabetical/Priority mapping for dispatching) */
static const KeywordEntry KEYWORD_TABLE[] = {
    {"PRINT", TOKEN_PRINT, do_print},
    {"LET",   TOKEN_LET,   do_let},
    {"GOTO",  TOKEN_GOTO,  do_goto},
    {"IF",    TOKEN_IF,    do_if},
    {"THEN",  TOKEN_THEN,  NULL},
    {"INPUT", TOKEN_INPUT, do_input},
    {"END",   TOKEN_END,   do_end},
    {"LIST",  TOKEN_LIST,  NULL},
    {"RUN",   TOKEN_RUN,   NULL},
    {"CLEAR", TOKEN_CLEAR, NULL},
    {NULL,    0,           NULL}
};

static void skip_spaces(const char **str) {
    while (**str == ' ' || **str == '\t') (*str)++;
}

static int evaluate_expression(const char **str);

static int parse_factor(const char **str) {
    skip_spaces(str);
    int result = 0;
    if (**str == '(') {
        (*str)++;
        result = evaluate_expression(str);
        skip_spaces(str);
        if (**str == ')') (*str)++;
    } else if (isdigit((unsigned char)**str)) {
        while (isdigit((unsigned char)**str)) {
            result = result * 10 + (**str - '0');
            (*str)++;
        }
    } else if (isalpha((unsigned char)**str)) {
        char var = toupper((unsigned char)**str) - 'A';
        result = variables[(int)var];
        (*str)++;
    }
    return result;
}

static int parse_term(const char **str) {
    skip_spaces(str);
    int result = parse_factor(str);
    skip_spaces(str);
    while (**str == '*' || **str == '/') {
        char op = **str;
        (*str)++;
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
        char op = **str;
        (*str)++;
        int next_term = parse_term(str);
        if (op == '+') result += next_term;
        else if (op == '-') result -= next_term;
        skip_spaces(str);
    }
    return result;
}

/* Helper to locate line indices */
static int find_line_index(int line_num) {
    for (int i = 0; i < line_count; i++) {
        if (program[i].line_number == line_num) return i;
    }
    return -1;
}

/* Keyword-to-Token Compressor */
static void tokenize_string(const char *in, char *out) {
    while (*in) {
        if (*in == '"') { /* Preserve string literal contents */
            *out++ = *in++;
            while (*in && *in != '"') *out++ = *in++;
            if (*in) *out++ = *in++;
            continue;
        }

        int matched = 0;
        for (int i = 0; KEYWORD_TABLE[i].keyword != NULL; i++) {
            size_t len = strlen(KEYWORD_TABLE[i].keyword);
            if (strncasecmp(in, KEYWORD_TABLE[i].keyword, len) == 0) {
                *out++ = (char)KEYWORD_TABLE[i].token;
                in += len;
                matched = 1;
                break;
            }
        }
        if (!matched) {
            *out++ = *in++;
        }
    }
    *out = '\0';
}

/* Detokenizer for the LIST command */
static void print_detokenized(const char *src) {
    while (*src) {
        uint8_t ch = (uint8_t)*src;
        if (ch >= 0x80) {
            for (int i = 0; KEYWORD_TABLE[i].keyword != NULL; i++) {
                if (KEYWORD_TABLE[i].token == ch) {
                    printf("%s", KEYWORD_TABLE[i].keyword);
                    break;
                }
            }
        } else {
            putchar(ch);
        }
        src++;
    }
    putchar('\n');
}

/* Table Dispatch Execution replacing direct IF/ELSEIF statements */
static void execute_statement(const char *src) {
    skip_spaces(&src);
    if (*src == '\0') return;

    uint8_t token = (uint8_t)*src;
    src++; /* Advance past token byte */

    for (int i = 0; KEYWORD_TABLE[i].keyword != NULL; i++) {
        if (KEYWORD_TABLE[i].token == token) {
            if (KEYWORD_TABLE[i].handler != NULL) {
                KEYWORD_TABLE[i].handler(&src);
            }
            return;
        }
    }
}

/* --- Individual Statement Handlers --- */

static void do_print(const char **src) {
    skip_spaces(src);
    if (**src == '"') {
        (*src)++;
        while (**src && **src != '"') putchar(*(*src)++);
        if (**src == '"') (*src)++;
        putchar('\n');
    } else {
        printf("%d\n", evaluate_expression(src));
    }
}

static void do_let(const char **src) {
    skip_spaces(src);
    if (isalpha((unsigned char)**src)) {
        int var = toupper((unsigned char)**src) - 'A';
        (*src)++;
        skip_spaces(src);
        if (**src == '=') {
            (*src)++;
            variables[var] = evaluate_expression(src);
        }
    }
}

static void do_goto(const char **src) {
    int target_line = evaluate_expression(src);
    int target_idx = find_line_index(target_line);
    if (target_idx != -1) {
        current_exec_index = target_idx;
    } else {
        printf("ERR: Line %d not found\n", target_line);
        current_exec_index = -1;
    }
}

static void do_if(const char **src) {
    int val1 = evaluate_expression(src);
    skip_spaces(src);

    char op = **src;
    if (op == '=' || op == '<' || op == '>') (*src)++;
    int val2 = evaluate_expression(src);

    int condition = 0;
    if (op == '=') condition = (val1 == val2);
    else if (op == '<') condition = (val1 < val2);
    else if (op == '>') condition = (val1 > val2);

    skip_spaces(src);
    if ((uint8_t)**src == TOKEN_THEN) {
        (*src)++;
        if (condition) execute_statement(*src);
    }
}

static void do_input(const char **src) {
    skip_spaces(src);
    if (isalpha((unsigned char)**src)) {
        int var = toupper((unsigned char)**src) - 'A';
        char buf[32];
        printf("? ");
        if (fgets(buf, sizeof(buf), stdin)) {
            variables[var] = atoi(buf);
        }
    }
}

static void do_end(const char **src) {
    (void)src;
    current_exec_index = -1;
}

/* --- Memory & Execution Engine --- */

static void store_line(int line_num, const char *text) {
    int idx = find_line_index(line_num);
    char tokenized_buf[MAX_LINE_LEN];
    tokenize_string(text, tokenized_buf);

    if (tokenized_buf[0] == '\0') {
        if (idx != -1) {
            for (int i = idx; i < line_count - 1; i++) program[i] = program[i + 1];
            line_count--;
        }
        return;
    }

    if (idx != -1) {
        strncpy(program[idx].text, tokenized_buf, MAX_LINE_LEN - 1);
    } else if (line_count < MAX_LINES) {
        int i = line_count - 1;
        while (i >= 0 && program[i].line_number > line_num) {
            program[i + 1] = program[i];
            i--;
        }
        program[i + 1].line_number = line_num;
        strncpy(program[i + 1].text, tokenized_buf, MAX_LINE_LEN - 1);
        line_count++;
    }
}

static void run_program(void) {
    current_exec_index = 0;
    while (current_exec_index >= 0 && current_exec_index < line_count) {
        int prev_index = current_exec_index;
        execute_statement(program[current_exec_index].text);
        if (current_exec_index == prev_index) {
            current_exec_index++;
        }
    }
}

int main(void) {
    char input_buffer[MAX_LINE_LEN];
    printf("\n--- PIC32 Tokenized BASIC ---\nReady\n");

    while (1) {
        printf("> ");
        if (!fgets(input_buffer, sizeof(input_buffer), stdin)) break;

        input_buffer[strcspn(input_buffer, "\r\n")] = 0;
        const char *ptr = input_buffer;
        skip_spaces(&ptr);

        if (isdigit((unsigned char)*ptr)) {
            int line_num = atoi(ptr);
            while (isdigit((unsigned char)*ptr)) ptr++;
            skip_spaces(&ptr);
            store_line(line_num, ptr);
        } else if (strcasecmp(ptr, "RUN") == 0) {
            run_program();
        } else if (strcasecmp(ptr, "LIST") == 0) {
            for (int i = 0; i < line_count; i++) {
                printf("%d ", program[i].line_number);
                print_detokenized(program[i].text);
            }
        } else if (strcasecmp(ptr, "CLEAR") == 0) {
            line_count = 0;
            memset(variables, 0, sizeof(variables));
        } else {
            char tokenized[MAX_LINE_LEN];
            tokenize_string(ptr, tokenized);
            execute_statement(tokenized);
        }
    }
    return 0;
}