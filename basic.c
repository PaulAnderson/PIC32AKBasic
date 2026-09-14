#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINES 100
#define MAX_LINE_LEN 80

typedef struct {
    int line_number;
    char text[MAX_LINE_LEN];
} ProgramLine;

static ProgramLine program[MAX_LINES];
static int line_count = 0;
static int variables[26];
static int current_exec_index = -1;

/* Forward declarations */
static void execute_statement(const char *src);
static int evaluate_expression(const char **str);

/* Skip whitespace */
static void skip_spaces(const char **str) {
    while (**str == ' ' || **str == '\t') {
        (*str)++;
    }
}

/* Factor parsing: numbers, variables, or parenthesized expressions */
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

/* Term parsing: multiplication and division */
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

/* Expression parsing: addition and subtraction */
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

/* Finds index of a given line number in program memory */
static int find_line_index(int line_num) {
    for (int i = 0; i < line_count; i++) {
        if (program[i].line_number == line_num) return i;
    }
    return -1;
}

/* Program memory management */
static void store_line(int line_num, const char *text) {
    int idx = find_line_index(line_num);

    /* Delete line if empty text */
    if (text[0] == '\0') {
        if (idx != -1) {
            for (int i = idx; i < line_count - 1; i++) program[i] = program[i + 1];
            line_count--;
        }
        return;
    }

    /* Insert or update line */
    if (idx != -1) {
        strncpy(program[idx].text, text, MAX_LINE_LEN - 1);
        program[idx].text[MAX_LINE_LEN - 1] = '\0';
    } else if (line_count < MAX_LINES) {
        int i = line_count - 1;
        while (i >= 0 && program[i].line_number > line_num) {
            program[i + 1] = program[i];
            i--;
        }
        program[i + 1].line_number = line_num;
        strncpy(program[i + 1].text, text, MAX_LINE_LEN - 1);
        program[i + 1].text[MAX_LINE_LEN - 1] = '\0';
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

/* Statement execution logic */
static void execute_statement(const char *src) {
    skip_spaces(&src);
    if (*src == '\0') return;

    if (strncasecmp(src, "PRINT", 5) == 0) {
        src += 5;
        skip_spaces(&src);
        if (*src == '"') {
            src++;
            while (*src && *src != '"') putchar(*src++);
            if (*src == '"') src++;
            putchar('\n');
        } else {
            printf("%d\n", evaluate_expression(&src));
        }
    } else if (strncasecmp(src, "LET", 3) == 0) {
        src += 3;
        skip_spaces(&src);
        if (isalpha((unsigned char)*src)) {
            int var = toupper((unsigned char)*src) - 'A';
            src++;
            skip_spaces(&src);
            if (*src == '=') {
                src++;
                variables[var] = evaluate_expression(&src);
            }
        }
    } else if (strncasecmp(src, "GOTO", 4) == 0) {
        src += 4;
        int target_line = evaluate_expression(&src);
        int target_idx = find_line_index(target_line);
        if (target_idx != -1) {
            current_exec_index = target_idx;
        } else {
            printf("ERR: Line %d not found\n", target_line);
            current_exec_index = -1;
        }
    } else if (strncasecmp(src, "IF", 2) == 0) {
        src += 2;
        int val1 = evaluate_expression(&src);
        skip_spaces(&src);

        char op = *src;
        if (op == '=' || op == '<' || op == '>') src++;
        int val2 = evaluate_expression(&src);

        int condition = 0;
        if (op == '=') condition = (val1 == val2);
        else if (op == '<') condition = (val1 < val2);
        else if (op == '>') condition = (val1 > val2);

        skip_spaces(&src);
        if (strncasecmp(src, "THEN", 4) == 0) {
            src += 4;
            if (condition) execute_statement(src);
        }
    } else if (strncasecmp(src, "INPUT", 5) == 0) {
        src += 5;
        skip_spaces(&src);
        if (isalpha((unsigned char)*src)) {
            int var = toupper((unsigned char)*src) - 'A';
            char buf[32];
            printf("? ");
            if (fgets(buf, sizeof(buf), stdin)) {
                variables[var] = atoi(buf);
            }
        }
    } else if (strncasecmp(src, "END", 3) == 0) {
        current_exec_index = -1;
    }
}

int main(void) {
    char input_buffer[MAX_LINE_LEN];
    printf("\n--- PIC32 Tiny BASIC ---\nReady\n");

    while (1) {
        printf("> ");
        if (!fgets(input_buffer, sizeof(input_buffer), stdin)) break;

        /* Strip trailing newlines */
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
                printf("%d %s\n", program[i].line_number, program[i].text);
            }
        } else if (strcasecmp(ptr, "CLEAR") == 0) {
            line_count = 0;
            memset(variables, 0, sizeof(variables));
        } else {
            execute_statement(ptr);
        }
    }
    return 0;
}
