/*
 SScript Standard Library
 short_cast.ss
*/

@("stdlib\\numbers.ss")
@visibility("public")

/*
 bool, char, int, float, string
*/

namespace std
{

/* bool */
function<bool> b2b(bool b)
{
 return b;
}

function<bool> c2b(char c)
{
 return cast<bool>(c);
}

function<bool> i2b(int i)
{
 return cast<bool>(i);
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
 return cast<int>(b);
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
 return cast<int>(f);
}

function<char> s2c(string s)
{
 return s[1];
}

/* int */
function<int> b2i(bool b)
{
 return cast<int>(b);
}

function<int> c2i(char c)
{
 return cast<int>(c);
}

function<int> i2i(int i)
{
 return i;
}

function<int> f2i(float f)
{
 return cast<int>(f);
}

function<int> s2i(string s)
{
 return strint(s);
}

/* float */
function<float> b2f(bool b)
{
 return cast<int>(b);
}

function<float> c2f(char c)
{
 return cast<int>(c);
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

/* string */
function<string> b2s(bool b)
{
 if (b)
  return "true"; else
  return "false";
}

function<string> c2s(char c)
{
 return cast<string>(c);
}

function<string> i2s(int i)
{
 return intstr(i);
}

function<string> f2s(float f)
{
 return fltstr(f);
}

function<string> s2s(string s)
{
 return s;
}

}