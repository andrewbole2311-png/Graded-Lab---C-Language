// Andrea Bole 260adb177
// https://github.com/andrewbole2311-png/Graded-Lab---C-Language
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: Gemini (recursive descent parser architecture, robust memory management, float evaluation)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>

#define STUDENT_NAME     "andrea"
#define STUDENT_LASTNAME "bole"
#define STUDENT_ID       "260adb177"

// Here we create a type to store numbers and handle both regular integers and floating-point numbers.
// We use this simple enum so we directly know what kind of number we are dealing with without any headache.
typedef enum {
    VAL_INT,
    VAL_FLOAT
} ValType;

typedef struct {
    ValType type;
    long long i_val;
    double d_val;
} Value;

// All the possible pieces we can find in a line (the tokens):
// numbers, operators (+, -, *, /, **), parentheses, and end-of-line.
typedef enum {
    TOK_INT,
    TOK_FLOAT,
    TOK_PLUS,
    TOK_MINUS,
    TOK_MUL,
    TOK_DIV,
    TOK_POW,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_EOL
} TokenType;

// In each token, we save its true absolute 1-based position in the file.
// That way, if there is a syntax mistake or typo, we can point directly to the exact character.
typedef struct {
    TokenType type;
    long long i_val;
    double d_val;
    long pos;
} Token;

typedef struct {
    Token *tokens;
    size_t count;
    size_t capacity;
    size_t cursor;
    bool has_error;
    long error_pos;
} ParserState;

// As soon as we catch the first error, we record it.
// If has_error is already true, we don't overwrite anything since we only report the very first error.
static void set_error(ParserState *ps, long pos) {
    if (!ps->has_error) {
        ps->has_error = true;
        ps->error_pos = pos;
    }
}

static Value parse_expr(ParserState *ps);
static Value parse_term(ParserState *ps);
static Value parse_power(ParserState *ps);
static Value parse_primary(ParserState *ps);

static Token current_tok(ParserState *ps) {
    if (ps->cursor < ps->count) return ps->tokens[ps->cursor];
    return ps->tokens[ps->count - 1];
}

static Token advance_tok(ParserState *ps) {
    Token t = current_tok(ps);
    if (ps->cursor < ps->count - 1) {
        ps->cursor++;
    }
    return t;
}

// Bottom level of the hierarchy (highest precedence):
// Either a raw number (integer or float), or a sub-expression wrapped inside parentheses.
static Value parse_primary(ParserState *ps) {
    Value res = { VAL_INT, 0, 0.0 };
    if (ps->has_error) return res;

    Token t = current_tok(ps);
    if (t.type == TOK_INT) {
        advance_tok(ps);
        res.type = VAL_INT;
        res.i_val = t.i_val;
        return res;
    }
    if (t.type == TOK_FLOAT) {
        advance_tok(ps);
        res.type = VAL_FLOAT;
        res.d_val = t.d_val;
        return res;
    }
    if (t.type == TOK_LPAREN) {
        advance_tok(ps);
        res = parse_expr(ps);
        if (ps->has_error) return res;

        Token close = current_tok(ps);
        if (close.type == TOK_RPAREN) {
            advance_tok(ps);
        } else {
            set_error(ps, close.pos);
        }
        return res;
    }

    set_error(ps, t.pos);
    return res;
}

static long long int_pow(long long base, long long exp) {
    long long res = 1;
    while (exp > 0) {
        if (exp & 1) res *= base;
        base *= base;
        exp >>= 1;
    }
    return res;
}

// Handling power (**).
static Value parse_power(ParserState *ps) {
    Value left = parse_primary(ps);
    if (ps->has_error) return left;

    if (current_tok(ps).type == TOK_POW) {
        Token op = advance_tok(ps);
        long exp_pos = current_tok(ps).pos;
        Value right = parse_power(ps); // Recursive call to the right to respect right-associativity
        if (ps->has_error) return left;

        // If both operands are integers and the exponent is negative:
        // this is forbidden for integers, and the error must point to the start of the exponent.
        if (left.type == VAL_INT && right.type == VAL_INT) {
            if (right.i_val < 0) {
                set_error(ps, exp_pos);
                return left;
            }
            left.i_val = int_pow(left.i_val, right.i_val);
            return left;
        }

        // Otherwise, as soon as one operand is a float, we promote everything to double.
        double base_d = (left.type == VAL_INT) ? (double)left.i_val : left.d_val;
        double exp_d = (right.type == VAL_INT) ? (double)right.i_val : right.d_val;
        double result = pow(base_d, exp_d);

        // If the calculation blows up (like an invalid float result or infinity), the error points to **.
        if (isnan(result) || isinf(result)) {
            set_error(ps, op.pos);
            return left;
        }
        left.type = VAL_FLOAT;
        left.d_val = result;
    }
    return left;
}

// Handling multiplication and division (* and /).
static Value parse_term(ParserState *ps) {
    Value left = parse_power(ps);
    if (ps->has_error) return left;

    while (current_tok(ps).type == TOK_MUL || current_tok(ps).type == TOK_DIV) {
        Token op = advance_tok(ps);
        long right_pos = current_tok(ps).pos;
        Value right = parse_power(ps);
        if (ps->has_error) return left;

        if (op.type == TOK_MUL) {
            if (left.type == VAL_INT && right.type == VAL_INT) {
                left.i_val *= right.i_val;
            } else {
                double l = (left.type == VAL_INT) ? (double)left.i_val : left.d_val;
                double r = (right.type == VAL_INT) ? (double)right.i_val : right.d_val;
                left.type = VAL_FLOAT;
                left.d_val = l * r;
            }
        } else {
            // Check for division by zero.
            if (right.type == VAL_INT) {
                if (right.i_val == 0) {
                    set_error(ps, right_pos);
                    return left;
                }
            } else {
                if (right.d_val == 0.0) {
                    set_error(ps, right_pos);
                    return left;
                }
            }

            if (left.type == VAL_INT && right.type == VAL_INT) {
                left.i_val /= right.i_val; // Standard integer division truncating toward zero
            } else {
                double l = (left.type == VAL_INT) ? (double)left.i_val : left.d_val;
                double r = (right.type == VAL_INT) ? (double)right.i_val : right.d_val;
                left.type = VAL_FLOAT;
                left.d_val = l / r;
            }
        }
    }
    return left;
}

// Lowest level of grammar (+ and -).
// Just like term, this evaluates from left to right using a while loop.
static Value parse_expr(ParserState *ps) {
    Value left = parse_term(ps);
    if (ps->has_error) return left;

    while (current_tok(ps).type == TOK_PLUS || current_tok(ps).type == TOK_MINUS) {
        Token op = advance_tok(ps);
        Value right = parse_term(ps);
        if (ps->has_error) return left;

        if (left.type == VAL_INT && right.type == VAL_INT) {
            if (op.type == TOK_PLUS) left.i_val += right.i_val;
            else left.i_val -= right.i_val;
        } else {
            // As soon as there is a float in the expression, promote to double
            double l = (left.type == VAL_INT) ? (double)left.i_val : left.d_val;
            double r = (right.type == VAL_INT) ? (double)right.i_val : right.d_val;
            left.type = VAL_FLOAT;
            if (op.type == TOK_PLUS) left.d_val = l + r;
            else left.d_val = l - r;
        }
    }
    return left;
}

// Helper to store tokens in memory as we split up the line
static void add_token(ParserState *ps, Token t) {
    if (ps->count >= ps->capacity) {
        ps->capacity = ps->capacity ? ps->capacity * 2 : 32;
        Token *new_tokens = realloc(ps->tokens, ps->capacity * sizeof(Token));
        if (!new_tokens) {
            exit(1);
        }
        ps->tokens = new_tokens;
    }
    ps->tokens[ps->count++] = t;
}

// The Tokenizer reads the text character by character.
// We pass it the starting line position in the entire file (start_pos).
// That way, every character gets its exact 1-based address from the start of the file.
static bool tokenize_line(const char *line, size_t len, long start_pos, ParserState *ps) {
    ps->count = 0;
    ps->cursor = 0;
    size_t i = 0;

    while (i < len) {
        char c = line[i];
        long cur_file_pos = start_pos + (long)i;

        // Skip spaces and tabs as required
        if (c == ' ' || c == '\t' || c == '\r') {
            i++;
            continue;
        }

        if (c == '+') {
            add_token(ps, (Token){ TOK_PLUS, 0, 0.0, cur_file_pos });
            i++;
        } else if (c == '-') {
            add_token(ps, (Token){ TOK_MINUS, 0, 0.0, cur_file_pos });
            i++;
        } else if (c == '*') {
            // If we see two asterisks in a row, it is the power operator **
            if (i + 1 < len && line[i + 1] == '*') {
                add_token(ps, (Token){ TOK_POW, 0, 0.0, cur_file_pos });
                i += 2;
            } else {
                add_token(ps, (Token){ TOK_MUL, 0, 0.0, cur_file_pos });
                i++;
            }
        } else if (c == '/') {
            add_token(ps, (Token){ TOK_DIV, 0, 0.0, cur_file_pos });
            i++;
        } else if (c == '(') {
            add_token(ps, (Token){ TOK_LPAREN, 0, 0.0, cur_file_pos });
            i++;
        } else if (c == ')') {
            add_token(ps, (Token){ TOK_RPAREN, 0, 0.0, cur_file_pos });
            i++;
        } else if (isdigit((unsigned char)c)) {
            size_t start = i;
            while (i < len && isdigit((unsigned char)line[i])) i++;

            // Strict floating-point validation rules:
            // There must be digits after the dot (e.g. 12.5 is valid, but 5. is not).
            if (i < len && line[i] == '.') {
                if (i + 1 < len && isdigit((unsigned char)line[i + 1])) {
                    i++;
                    while (i < len && isdigit((unsigned char)line[i])) i++;
                    // Scientific notation like 1.5e3 is not allowed
                    if (i < len && (line[i] == 'e' || line[i] == 'E')) {
                        set_error(ps, start_pos + (long)i);
                        return false;
                    }
                    char buf[128];
                    size_t tok_len = i - start;
                    if (tok_len >= sizeof(buf)) tok_len = sizeof(buf) - 1;
                    memcpy(buf, line + start, tok_len);
                    buf[tok_len] = '\0';
                    char *endptr = NULL;
                    double d = strtod(buf, &endptr);
                    add_token(ps, (Token){ TOK_FLOAT, 0, d, cur_file_pos });
                } else {
                    set_error(ps, start_pos + (long)i);
                    return false;
                }
            } else {
                long long val = 0;
                for (size_t k = start; k < i; ++k) {
                    val = val * 10 + (line[k] - '0');
                }
                add_token(ps, (Token){ TOK_INT, val, 0.0, cur_file_pos });
            }
        } else {
            // Any other invalid symbol.
            set_error(ps, cur_file_pos);
            return false;
        }
    }

    add_token(ps, (Token){ TOK_EOL, 0, 0.0, start_pos + (long)len });
    return true;
}

// Function to recursively create parent folders
static bool ensure_directory_exists(const char *path) {
    char temp[2048];
    snprintf(temp, sizeof(temp), "%s", path);
    size_t len = strlen(temp);
    if (len == 0) return true;
    if (temp[len - 1] == '/') temp[len - 1] = '\0';

    for (char *p = temp + 1; *p; p++) {
        if (*p == '/') {
            *p = '\0';
            struct stat st;
            if (stat(temp, &st) != 0) {
#if defined(_WIN32)
                mkdir(temp);
#else
                mkdir(temp, 0777);
#endif
            }
            *p = '/';
        }
    }

    struct stat st;
    if (stat(temp, &st) != 0) {
#if defined(_WIN32)
        if (mkdir(temp) != 0 && errno != EEXIST) return false;
#else
        if (mkdir(temp, 0777) != 0 && errno != EEXIST) return false;
#endif
    }
    return true;
}

// Here we process an entire file.
// To avoid slowing down on multi-megabyte files, we read the entire file into memory at once.
bool process_single_file(const char *in_filepath, const char *out_filepath) {
    FILE *in = fopen(in_filepath, "rb");
    if (!in) {
        return false;
    }

    fseek(in, 0, SEEK_END);
    long fsize = ftell(in);
    fseek(in, 0, SEEK_SET);

    char *buffer = malloc((size_t)fsize + 2);
    if (!buffer && fsize > 0) {
        fclose(in);
        return false;
    }
    if (fsize > 0 && fread(buffer, 1, (size_t)fsize, in) != (size_t)fsize) {
        free(buffer);
        fclose(in);
        return false;
    }
    fclose(in);
    buffer[fsize] = '\0';
    size_t out_cap = 1024;
    size_t out_len = 0;
    char *out_buffer = malloc(out_cap);
    if (!out_buffer) {
        free(buffer);
        return false;
    }
    out_buffer[0] = '\0';

    ParserState ps = { NULL, 0, 0, 0, false, 0 };
    long first_error_pos = -1;

    size_t offset = 0;
    while (offset < (size_t)fsize) {
        size_t line_start = offset;
        while (offset < (size_t)fsize && buffer[offset] != '\n') {
            offset++;
        }
        size_t line_len = offset - line_start;
        long line_pos_1based = (long)line_start + 1;

        if (offset < (size_t)fsize && buffer[offset] == '\n') {
            offset++;
        }

        // Check if the line is blank or starts with a comment
        size_t non_blank = 0;
        while (non_blank < line_len && (buffer[line_start + non_blank] == ' ' || buffer[line_start + non_blank] == '\t' || buffer[line_start + non_blank] == '\r')) {
            non_blank++;
        }

        if (non_blank == line_len) continue;
        if (buffer[line_start + non_blank] == '#') continue;

        if (first_error_pos == -1) {
            ps.has_error = false;
            if (!tokenize_line(buffer + line_start, line_len, line_pos_1based, &ps)) {
                first_error_pos = ps.error_pos;
            } else {
                Value val = parse_expr(&ps);
                if (ps.has_error) {
                    first_error_pos = ps.error_pos;
                } else if (current_tok(&ps).type != TOK_EOL) {
                    first_error_pos = current_tok(&ps).pos;
                } else {
                    char res_buf[64];
                    int written;
                    // Format integers with %lld and floats with %.15g.
                    if (val.type == VAL_INT) {
                        written = snprintf(res_buf, sizeof(res_buf), "%lld\n", val.i_val);
                    } else {
                        written = snprintf(res_buf, sizeof(res_buf), "%.15g\n", val.d_val);
                    }

                    if (written > 0) {
                        while (out_len + (size_t)written + 1 >= out_cap) {
                            out_cap *= 2;
                            char *new_buf = realloc(out_buffer, out_cap);
                            if (!new_buf) {
                                free(out_buffer);
                                free(ps.tokens);
                                free(buffer);
                                return false;
                            }
                            out_buffer = new_buf;
                        }
                        memcpy(out_buffer + out_len, res_buf, (size_t)written);
                        out_len += (size_t)written;
                        out_buffer[out_len] = '\0';
                    }
                }
            }
        }
    }

    free(ps.tokens);
    free(buffer);

    // Make sure the destination directory exists before writing the file
    char out_dir_copy[2048];
    snprintf(out_dir_copy, sizeof(out_dir_copy), "%s", out_filepath);
    char *last_slash = strrchr(out_dir_copy, '/');
    if (last_slash) {
        *last_slash = '\0';
        ensure_directory_exists(out_dir_copy);
    }

    FILE *out = fopen(out_filepath, "wb");
    if (!out) {
        free(out_buffer);
        return false;
    }

    // "All-or-nothing" rule:
    // If any error occurs anywhere in the file, do not print partial results,
    // only write ERROR:<pos>.
    if (first_error_pos != -1) {
        fprintf(out, "ERROR:%ld\n", first_error_pos);
    } else {
        if (out_len > 0) {
            fwrite(out_buffer, 1, out_len, out);
        }
    }

    fclose(out);
    free(out_buffer);
    return true;
}

static void extract_base_name(const char *path, char *dest, size_t dest_sz) {
    const char *last_slash = strrchr(path, '/');
#if defined(_WIN32)
    const char *last_bslash = strrchr(path, '\\');
    if (!last_slash || (last_bslash && last_bslash > last_slash)) {
        last_slash = last_bslash;
    }
#endif
    const char *filename = last_slash ? last_slash + 1 : path;

    if (*filename == '\0' && last_slash != path) {
        char temp[1024];
        size_t len = (size_t)(last_slash - path);
        if (len >= sizeof(temp)) len = sizeof(temp) - 1;
        strncpy(temp, path, len);
        temp[len] = '\0';
        extract_base_name(temp, dest, dest_sz);
        return;
    }

    strncpy(dest, filename, dest_sz - 1);
    dest[dest_sz - 1] = '\0';

    size_t len = strlen(dest);
    if (len >= 4 && strcmp(dest + len - 4, ".txt") == 0) {
        dest[len - 4] = '\0';
    }
}

static bool ends_with_txt(const char *str) {
    size_t len = strlen(str);
    if (len < 4) return false;
    return strcmp(str + len - 4, ".txt") == 0;
}

static int cmp_strings(const void *a, const void *b) {
    return strcmp(*(const char **)a, *(const char **)b);
}

// Function to process an entire directory when the user passes the -d option
static bool process_directory(const char *dir_path, const char *out_dir) {
    DIR *d = opendir(dir_path);
    if (!d) return false;

    if (!ensure_directory_exists(out_dir)) {
        closedir(d);
        return false;
    }

    char **file_list = NULL;
    size_t count = 0, capacity = 0;
    struct dirent *entry;

    while ((entry = readdir(d)) != NULL) {
        if (!ends_with_txt(entry->d_name)) continue;

        char full_in[2048];
        snprintf(full_in, sizeof(full_in), "%s/%s", dir_path, entry->d_name);
        struct stat st;
        if (stat(full_in, &st) != 0 || !S_ISREG(st.st_mode)) continue;

        if (count >= capacity) {
            capacity = capacity ? capacity * 2 : 16;
            char **new_list = realloc(file_list, capacity * sizeof(char *));
            if (!new_list) {
                closedir(d);
                for (size_t i = 0; i < count; ++i) free(file_list[i]);
                free(file_list);
                return false;
            }
            file_list = new_list;
        }

        file_list[count] = strdup(entry->d_name);
        count++;
    }
    closedir(d);

    qsort(file_list, count, sizeof(char *), cmp_strings);

    bool all_ok = true;
    for (size_t i = 0; i < count; ++i) {
        char base[512];
        extract_base_name(file_list[i], base, sizeof(base));

        char full_in[2048];
        char full_out[2048];
        snprintf(full_in, sizeof(full_in), "%s/%s", dir_path, file_list[i]);
        snprintf(full_out, sizeof(full_out), "%s/%s_%s_%s_%s.txt",
                 out_dir, base, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);

        if (!process_single_file(full_in, full_out)) {
            all_ok = false;
        }
        free(file_list[i]);
    }
    free(file_list);

    return all_ok;
}

// Program entry point handling command-line arguments
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n", argv[0]);
        return 1;
    }

    const char *dir_arg = NULL;
    const char *out_arg = NULL;
    const char *input_file = NULL;

    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (i + 1 >= argc) return 1;
            dir_arg = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (i + 1 >= argc) return 1;
            out_arg = argv[++i];
        } else if (argv[i][0] == '-') {
            return 1;
        } else {
            if (input_file != NULL) return 1;
            input_file = argv[i];
        }
    }

    if (!dir_arg && !input_file) return 1;
    if (dir_arg && input_file) return 1;

    char default_out_dir[1024];
    const char *final_out_dir = out_arg;

    // If the user does not provide an -o option, use the default folder
    if (!final_out_dir) {
        char base[512];
        extract_base_name(dir_arg ? dir_arg : input_file, base, sizeof(base));
        snprintf(default_out_dir, sizeof(default_out_dir), "%s_%s_%s",
                 base, STUDENT_NAME, STUDENT_ID);
        final_out_dir = default_out_dir;
    }

    if (dir_arg) {
        return process_directory(dir_arg, final_out_dir) ? 0 : 1;
    }

    if (!ensure_directory_exists(final_out_dir)) return 1;

    char base[512];
    extract_base_name(input_file, base, sizeof(base));

    char full_out[2048];
    snprintf(full_out, sizeof(full_out), "%s/%s_%s_%s_%s.txt",
             final_out_dir, base, STUDENT_NAME, STUDENT_LASTNAME, STUDENT_ID);

    return process_single_file(input_file, full_out) ? 0 : 1;
}