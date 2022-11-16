# Abstract
Pretty-printers have been studied for over 50 years, and many effcient algorithms have been proposed and implemented. Algebras have been developed
to describe pretty-printers formally, and pretty-printer libraries have been developed from those algebras. However, far less work was done to improve the
capabilities of pretty-printers.

We propose that considering pretty-printing as a form of compilation simplifies the work required to construct a pretty-printer and allows programmers to define formatting specifications not possible previously. Pretty-printing code is equivalent to compiling text in some formal language onto another representation of the same text in the same language such that the output conforms to a set of predefined layout specifications. We will show that using an intermediate representation made up of two-dimensional boxes is suffcient and appropriate for describing the spacial layout of the formatted source code.

We propose a DSL with which users may define format specifications. We
claim that, when used along with the grammar of the formatted language, this
DSL is suffcient to generate pretty-printers that formats code by compiling
it. To demonstrate these claims, we developed a system for generating such
pretty-printers, and with it, we create pretty-printers for various languages
and specifications.

We do not study the complexity of pretty-printers since we reduce the computational complexity of pretty-printing to that of compiling. Instead, we
concentrate on generalizing the capabilities of pretty-printers and reducing
the work required when creating a pretty-printer for a new language or a new
formatting specification for and existing language.
