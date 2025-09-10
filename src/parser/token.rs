// Lua keywords:
// and       break     do        else      elseif    end
// false     for       function  goto      if        in
// local     nil       not       or        repeat    return
// then      true      until     while
//
// Other tokens:
// +     -     *     /     %     ^     #
// &     ~     |     <<    >>    //
// ==    ~=    <=    >=    <     >     =
// (     )     {     }     [     ]     ::
// ;     :     ,     .     ..    ...
//
// Data tokens:
// nil, boolean, number (integer and float), and string
// > Note: function, userdata, thread, and table are not parsed as single tokens

// pub enum LuzToken {
//     Whitespace,
//     Comment,
//
//     // Data
//     Nil,
//     Boolean(bool),
//     Integer(i64),
//     Float(f64),
//     String(String),
//
//     // Operators
//     Plus,
//     Minus,
//     Star,
//     Slash,
//     Percent,
//     Caret,
//     Hash,
//
//     Ampersand,
//     Tilde,
//     Pipe,
//     ShiftLeft,
//     ShiftRight,
//     DoubleSlash,
//
//     Eq,
//     Neq,
//     Le,
//     Ge,
//     Lt,
//     Gt,
//
//     Assign,
//
//     ParenOpen,
//     ParentClose,
//     CurlyOpen,
//     CurlyClose,
//     SquareOpen,
//     SquareClose,
//
//     DoubleColon,
//     Semicolon,
//     Colon,
//     Comma,
//     Dot,
//     DoubleDot,
//     TripleDot,
//
//     // Keywords
//     KwAnd,
//     KwBreak,
//     KwDo,
//     KwElse,
//     KwElseif,
//     KwEnd,
//     // KwFalse,
//     KwFor,
//     KwFunction,
//     KwGoto,
//     KwIf,
//     KwIn,
//     KwLocal,
//     // KwNil,
//     KwNot,
//     KwOr,
//     KwRepeat,
//     KwReturn,
//     KwThen,
//     // KwTrue,
//     KwUntil,
//     KwWhile,
// }
//
// impl LuzToken {}
