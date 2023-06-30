/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     VOID = 258,
     INT = 259,
     BYTE = 260,
     B = 261,
     BOOL = 262,
     TRUE = 263,
     FALSE = 264,
     RETURN = 265,
     ID = 266,
     NUM = 267,
     STRING = 268,
     SC = 269,
     COMMA = 270,
     COMMENT = 271,
     WHILE = 272,
     BREAK = 273,
     CONTINUE = 274,
     LBRACE = 275,
     RBRACE = 276,
     S = 277,
     OVERRIDE = 278,
     ASSIGN = 279,
     OR = 280,
     AND = 281,
     RELOP = 282,
     BINOP_MULT = 283,
     BINOP_PLUS = 284,
     NOT = 285,
     LPAREN = 286,
     RPAREN = 287,
     IF = 288,
     ELSE = 289
   };
#endif
/* Tokens.  */
#define VOID 258
#define INT 259
#define BYTE 260
#define B 261
#define BOOL 262
#define TRUE 263
#define FALSE 264
#define RETURN 265
#define ID 266
#define NUM 267
#define STRING 268
#define SC 269
#define COMMA 270
#define COMMENT 271
#define WHILE 272
#define BREAK 273
#define CONTINUE 274
#define LBRACE 275
#define RBRACE 276
#define S 277
#define OVERRIDE 278
#define ASSIGN 279
#define OR 280
#define AND 281
#define RELOP 282
#define BINOP_MULT 283
#define BINOP_PLUS 284
#define NOT 285
#define LPAREN 286
#define RPAREN 287
#define IF 288
#define ELSE 289




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

