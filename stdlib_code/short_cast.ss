/*
 SScript Standard Library
 short_cast.ss
*/

@("stdlib\\numbers.ss")
@visibility("public")

/*
 bool, char, int, float, string
*/

/* bool */
function<bool> b2b(bool b)
{
 return b;
}

function<bool> c2b(char c)
{
 return bool(c);
}

function<bool> i2b(int i)
{
 return bool(i);
}

function<bool> f2b(float f)
{
 return (f == 1);
}

function<bool> s2b(string s)
{
 return (s == "true");
}

/* char */
function<char> b2c(bool b)
{
 return int(b);
}

function<char> c2c(char c)
{
 return c;
}

function<char> i2c(int i)
{
 return i;
}

function<char> f2c(float f)
{
 return int(f);
}

function<char> s2c(string s)
{
 return s[1];
}

/* int */
function<int> b2i(bool b)
{
 return int(b);
}

function<int> c2i(char c)
{
 return int(c);
}

function<int> i2i(int i)
{
 return i;
}

function<int> f2i(float f)
{
 return int(f);
}

function<int> s2i(string s)
{
 return strint(s);
}

/* float */
function<float> b2f(bool b)
{
 return int(b);
}

function<float> c2f(char c)
{
 return int(c);
}

function<float> i2f(int i)
{
 return i;
}

function<float> f2f(float f)
{
 return f;
}

function<float> s2f(string s)
{
 return strflt(s);
}
