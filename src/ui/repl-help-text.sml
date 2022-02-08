
structure ReplHelpText =
struct

    val version = "v0.1.0alpha"

    val versionText = version ^ "\n"

    val gplCopyrightNotice = "\
\豫言编程语言\n\
\Copyright (C) 2021-2022  <name of author>\n\
\\n\
\This program is free software: you can redistribute it and/or modify\n\
\it under the terms of the GNU General Public License as published by\n\
\the Free Software Foundation, either version 3 of the License, or\n\
\(at your option) any later version.\n\
\\n\
\This program is distributed in the hope that it will be useful,\n\
\but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
\MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
\GNU General Public License for more details.\n\
\\n\
\You should have received a copy of the GNU General Public License\n\
\along with this program.  If not, see <https://www.gnu.org/licenses/>.\n"

    val aboutText = gplCopyrightNotice

    val helpText = "豫言 ☯  ("^ version ^") 运行：  yy [options] [files]\n\
\其中 [options] 可以是\n" ^ CommandLineArgumentsText.optionDescription ^ "\n"


end