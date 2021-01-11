import stdlib/base;

list? x     = _typeof x == "List";
bool? x     = _typeof x == "Bool";
string? x   = _typeof x == "String";
num? x      = _typeof x == "Num";
path? x     = _typeof x == "Path";
flag? x     = _typeof x == "Flag";
