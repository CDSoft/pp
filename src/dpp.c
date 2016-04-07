/*###########################################################################
# dpp: Diagram PreProcessor (for Pandoc)                                    #
# --------------------------------------                                    #
#                                                                           #
# PP                                                                        #
# Copyright (C) 2015, 2016 Christophe Delord                                #
# http://www.cdsoft.fr/pp                                                   #
#                                                                           #
# This file is part of PP.                                                  #
#                                                                           #
# PP is free software: you can redistribute it and/or modify                #
# it under the terms of the GNU General Public License as published by      #
# the Free Software Foundation, either version 3 of the License, or         #
# (at your option) any later version.                                       #
#                                                                           #
# PP is distributed in the hope that it will be useful,                     #
# but WITHOUT ANY WARRANTY; without even the implied warranty of            #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
# GNU General Public License for more details.                              #
#                                                                           #
# You should have received a copy of the GNU General Public License         #
# along with PP.  If not, see <http://www.gnu.org/licenses/>.               #
###########################################################################*/

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#define LINE_SIZE 4096

#define MIN_TILDA 3

typedef enum {QUOTE, DIAGRAM, SCRIPT} kind_t;
typedef enum {NONE, GRAPHVIZ, PLANTUML, DITAA, SH, BASH, BAT, PYTHON, HASKELL} cmd_t;

typedef struct {char *name; kind_t kind; cmd_t type; char *header, *footer; char *ext;} command_t;

const command_t commands[] =
{
/*   name           kind            cmd         header          footer  ext     */

    {"quote",       QUOTE,          NONE,       "",             ""},

    {"dot",         DIAGRAM,        GRAPHVIZ,   "",             ""},
    {"neato",       DIAGRAM,        GRAPHVIZ,   "",             ""},
    {"twopi",       DIAGRAM,        GRAPHVIZ,   "",             ""},
    {"circo",       DIAGRAM,        GRAPHVIZ,   "",             ""},
    {"fdp",         DIAGRAM,        GRAPHVIZ,   "",             ""},
    {"sfdp",        DIAGRAM,        GRAPHVIZ,   "",             ""},
    {"patchwork",   DIAGRAM,        GRAPHVIZ,   "",             ""},
    {"osage",       DIAGRAM,        GRAPHVIZ,   "",             ""},

    {"uml",         DIAGRAM,        PLANTUML,   "@startuml\n",  "@enduml\n"},

    {"ditaa",       DIAGRAM,        DITAA,      "",             ""},

    {"sh",          SCRIPT,         SH,         "",             "",     ".sh"},
    {"bash",        SCRIPT,         BASH,       "",             "",     ".sh"},
    {"bat",         SCRIPT,         BAT,        "@echo off\n",  "",     ".bat"},
    {"python",      SCRIPT,         PYTHON,     "",             "",     ".py"},
    {"haskell",     SCRIPT,         HASKELL,    "",             "",     ".hs"},

    {NULL}
};

typedef enum {SAME, DIFFERENT} cmp_t;

#ifdef __MINGW32__
#define TEMP getenv("TEMP")
#define null "nul"
#else
#define TEMP "/tmp"
#define null "/dev/null"
#endif

extern unsigned char plantuml_jar[];
extern unsigned int plantuml_jar_len;

extern unsigned char ditaa_jar[];
extern unsigned int ditaa_jar_len;

/* reports an unexpected error and exit */
void unexpected(const char *fmt, ...)
{
    char buffer[LINE_SIZE];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buffer, LINE_SIZE, fmt, args);
    fprintf(stderr, "dpp - unexpected error: %s\n", buffer);
    va_end(args);
    exit(1);
}

/* removes the trailing spaces */
void strip(char *s)
{
    char *sp;
    for (sp = s+strlen(s)-1; sp >= s && isspace(*sp); sp--)
    {
        *sp = '\0';
    }
}

/* checks that a string is made of 3 or more '~' or '`' */
int istilda(const char *s)
{
    int n = 0;
    char c = *s;
    switch (c)
    {
        case '~':
        case '`':
            while (*s == c) { s++; n++; }
            break;
    }
    return n >= MIN_TILDA && *s == '\0';
}

/* check that two strings contain the same kind of block delimiter */
int sametilda(const char *s1, const char *s2)
{
    return /* istilda(s1) && */ (strcmp(s1, s2) == 0);
}

/* checks that a string is a valid command */
const command_t *getcmd(const char *s)
{
    int i;
    for (i = 0; commands[i].name; i++)
    {
        if (strcmp(s, commands[i].name) == 0)
        {
            return &commands[i];
        }
    }
    return NULL;
}

/* checks if a file exists */
int exists(const char *name)
{
    struct stat st;
    return stat(name, &st) == 0;
}

/* writes a builtin resource */
const char *resource(const char *name, const unsigned char *data, unsigned int len)
{
    static char path[LINE_SIZE];
    snprintf(path, LINE_SIZE, "%s/%s", TEMP, name);
    if (!exists(path))
    {
        FILE *f = fopen(path, "wb");
        if (!f) unexpected("can not create file %s", path);
        fwrite(data, (size_t)len, 1, f);
        fclose(f);
    }
    return path;
}

/* compares two files */
cmp_t diff(const char *name1, const char *name2)
{
    FILE *f1 = fopen(name1, "rb");
    FILE *f2 = fopen(name2, "rb");
    char buf1[LINE_SIZE];
    char buf2[LINE_SIZE];
    size_t n1;
    size_t n2;
    if (!f1 || !f2)
    {
        if (f1) fclose(f1);
        if (f2) fclose(f2);
        return DIFFERENT;
    }
    do
    {
        n1 = fread(buf1, sizeof(char), LINE_SIZE, f1);
        n2 = fread(buf2, sizeof(char), LINE_SIZE, f2);
        if (n1!=n2 || memcmp(buf1, buf2, n1)!=0)
        {
            fclose(f1);
            fclose(f2);
            return DIFFERENT;
        }
    } while (n1 != 0 || n2 != 0);
    fclose(f1);
    fclose(f2);
    return SAME;
}

/* writes a source block to a file */
cmp_t saveblock(const char *tilda, const char *name, const command_t *c)
{
    char line[LINE_SIZE];
    char tmpname[LINE_SIZE];
    FILE *f;
    snprintf(tmpname, LINE_SIZE, "%s.tmp", name);
    f = fopen(tmpname, "wt");
    if (!f) unexpected("can not create file %s", tmpname);
    fputs(c->header, f);
    while (fgets(line, LINE_SIZE, stdin))
    {
        strip(line);
        if (sametilda(tilda, line)) break;
        fputs(line, f);
        fputs("\n", f);
    }
    fputs(c->footer, f);
    fclose(f);
    if (diff(name, tmpname) == DIFFERENT)
    {
        remove(name);
        rename(tmpname, name);
        return DIFFERENT;
    }
    else
    {
        remove(tmpname);
        return SAME;
    }
}

/* emits a block verbatim */
void emitblock(const char *tilda, const command_t *c)
{
    char line[LINE_SIZE];
    fputs(c->header, stdout);
    while (fgets(line, LINE_SIZE, stdin))
    {
        strip(line);
        if (sametilda(tilda, line)) break;
        fputs(line, stdout);
        fputs("\n", stdout);
    }
    fputs(c->footer, stdout);
}

/* extract the actual path of the image and the link to put in the HTML code */
void splitimg(const char *img, char *png, char *link)
{
    int onlyinpath = 0;
    while (*img)
    {
        switch (*img)
        {
            case '[':
                onlyinpath = 1;
                break;
            case ']':
                onlyinpath = 0;
                break;
            default:
                *png++ = *img;
                if (!onlyinpath) *link++ = *img;
                break;
        }
        img++;
    }
    *png = *link = '\0';
}

/* process stdin, generates images and stdout */
int main(int argc, char *argv[])
{
    char line[LINE_SIZE];

    if (argc != 1)
    {
        fprintf(stderr, "Diagram Preprocessor (for Pandoc)\n"
                        "(C) Christophe Delord (http://cdsoft.fr/pp)\n\n"
                        "dpp just:\n"
                        "    - takes stdin\n"
                        "    - generates images with Graphviz, plantUML or ditaa\n"
                        "    - executes Bash, bat, Python and Haskell scripts\n"
                        "    - writes stdout\n"
                        "dpp has no option.\n"
                        "\nThis work is free and distributed under the GNU General Public License version 3.\n"
                        );
        exit(1);
    }

    while (fgets(line, LINE_SIZE, stdin))
    {
        char tilda[LINE_SIZE];
        char cmd[LINE_SIZE];
        char img[LINE_SIZE];
        int legend;
        int n;
        const command_t *c;
        /* Verbatim blocks */
        n = sscanf(line, "%s %s", tilda, cmd);
        if (!isspace(line[0]) && n == 2 && istilda(tilda) && (c=getcmd(cmd)) != NULL && c->kind == QUOTE)
        {
            emitblock(tilda, c);
            continue;
        }
        /* Graph or diagram block */
        n = sscanf(line, "%s %s %s %n", tilda, cmd, img, &legend);
        if (!isspace(line[0]) && n >= 3 && istilda(tilda) && (c=getcmd(cmd)) != NULL && c->kind == DIAGRAM)
        {
            char png[LINE_SIZE];
            char link[LINE_SIZE];
            char txt[LINE_SIZE];
            char commandline[LINE_SIZE];
            strip(&line[legend]);
            splitimg(img, png, link); /* [a/b/]c/d is split into a/b/c/d and c/d */
            snprintf(txt, LINE_SIZE, "%s%s", png, ".txt");
            strncat(png, ".png", LINE_SIZE);
            strncat(link, ".png", LINE_SIZE);
            if (saveblock(tilda,txt, c) == DIFFERENT || !exists(png))
            {
                switch (c->type)
                {
                    case GRAPHVIZ:
                    {
                        snprintf(commandline, LINE_SIZE, "%s -Tpng -o %s %s 1>"null, cmd, png, txt);
                        break;
                    }
                    case PLANTUML:
                    {
                        const char *plantuml = resource("plantuml.jar", plantuml_jar, plantuml_jar_len);
                        snprintf(commandline, LINE_SIZE, "java -jar %s -charset UTF-8 %s 1>"null, plantuml, txt);
                        break;
                    }
                    case DITAA:
                    {
                        const char *ditaa = resource("ditaa.jar", ditaa_jar, ditaa_jar_len);
                        snprintf(commandline, LINE_SIZE, "java -jar %s -e UTF-8 -o %s %s 1>"null, ditaa, txt, png);
                        break;
                    }
                    default:
                    {
                        unexpected("bad diagram type (%d)", c->type);
                    }
                }
                fflush(stdout);
                system(commandline);
            }
            /* The image shall be generated before the markdown output to be sure that Pandoc can read it */
            /* (the usage of pipe may give the link to pandoc before the external command is terminated)  */
            printf("![%s](%s)\n", &line[legend], link);
            continue;
        }
        /* Script block */
        n = sscanf(line, "%s %s", tilda, cmd);
        if (!isspace(line[0]) && n >= 2 && istilda(tilda) && (c=getcmd(cmd)) != NULL && c->kind == SCRIPT)
        {
            char script[LINE_SIZE];
            char commandline[LINE_SIZE];
            snprintf(script, LINE_SIZE, "%s/dppscript%d%s", TEMP, getpid(), c->ext);
            saveblock(tilda,script, c);
            switch (c->type)
            {
                case SH:
                {
                    snprintf(commandline, LINE_SIZE, "sh %s", script);
                    break;
                }
                case BASH:
                {
                    snprintf(commandline, LINE_SIZE, "bash %s", script);
                    break;
                }
                case BAT:
                {
#ifdef __MINGW32__
                    snprintf(commandline, LINE_SIZE, "cmd /c %s", script);
#else
                    snprintf(commandline, LINE_SIZE, "wine cmd /c %s", script);
#endif
                    break;
                }
                case PYTHON:
                {
                    snprintf(commandline, LINE_SIZE, "python %s", script);
                    break;
                }
                case HASKELL:
                {
                    snprintf(commandline, LINE_SIZE, "runhaskell %s", script);
                    break;
                }
                default:
                {
                    unexpected("bad script type (%d)", c->type);
                }
            }
            fflush(stdout);
            system(commandline);
            remove(script);
            continue;
        }
        /* text line */
        fputs(line, stdout);
    }
    return 0;
}
