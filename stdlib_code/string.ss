/*
 SScript Standard Library
 string.ss
*/

@visibility("public")

namespace std
{

/* ===== CHAR FUNCTIONS ===== */
function<char> chupper(char ch)
{
 if ((ch >= 'a') && (ch <= 'z'))
  return (ch-32);
  
 return ch;
}

function<char> chlower(char ch)
{
 if ((ch >= 'A') && (ch <= 'Z')) // A .. Z
  return (ch+32);
  
 return ch;
}

function<bool> isdigit(char ch)
{
 return ((ch >= '0') && (ch <= '9')); // 0..9
}

function<bool> isxdigit(char ch)
{
 ch = chlower(ch);
 return (isdigit(ch) || ((ch >= 'a') && (ch <= 'f'))); // 0..9, a..f/A..F
}

function<bool> isalpha(char ch)
{
 ch = chlower(ch);
 return ((ch >= 'a') && (ch <= 'z'));
}

function<bool> isalnum(char ch)
{
 return (isdigit(ch) || isalpha(ch));
}

function<bool> isspace(char ch)
{
 return ((ch == cast<char>(0)) || (ch == '\n') || (ch == '\r') || (ch == '\t') || (ch == '\v') || (ch == '\f') || (ch == ' '));
}

/* ===== STRING FUNCTIONS ===== */
function<int> strlen(string str) naked 
{
 :CODE
 {
  sub(stp, 1)
  icall("string.length")
  pop(ei1)
  add(stp, 2)
 }
}

function<int> strcount(string text, char sep)
{
 var<int> count = 0, len = strlen(text);

 for (var<int> i=1; i<=len; i++)
  if (text[i] == sep)
   count++;

 return count;
}

function<string> strreverse(string text)
{
 var<string> str = "";
 
 for (var<int> i=strlen(text); i>=1; i--)
  str += text[i];
  
 return str;
}

function<string> strcopy(string text, int begin, int length)
{
 if (begin < 0) // negative `begin`
  begin = strlen(text)+begin+1;

 if ((begin < 0) || (length < 0)) // invalid length
  return "";

 var<int> end = begin+length-1;

 if (end > strlen(text)) // too long
  end = strlen(text);

 var<string> result = "";
 for (var<int> i=begin; i<=end; i++) // copy string char-by-char
  result += text[i];

 return result;
}

function<string> strdelete(string text, int begin, int length)
{
 if (begin < 0)
  return strdelete(text, strlen(text)+begin+1, length);

 return strcopy(text, 1, begin-1)+
        strcopy(text, begin+length, strlen(text));
}

function<string> strdelete_begin(string text, int count)
{
 return strcopy(text, count, strlen(text));
}

function<string> strdelete_end(string text, int count)
{
 return strdelete(text, strlen(text)-count, count);
}

function<int> strpos_ex(string search, string text, int begin)
{                   
 var<int> index = 0;
 var<int> len = strlen(text);
 var<int> slen = strlen(search);
        
 if ((slen > len) || (slen == 0))
  return 0;
        
 if ((slen == len) && (search == text))
  return 1;

 for (var<int> i=begin; i<=len; i++)
  if (text[i] == search[1])
  {
   var<bool> found=true;

   for (var<int> q=1; q<=slen; q++)
    if (text[i+q-1] != search[q]) 
    { 
     found = false;
     break;
    }
     
   if (found)
   {
    index = i;
    break;
   }
  }

 return index;
}

function<int> strpos(string search, string text)
{                   
 return strpos_ex(search, text, 1);
}

function<bool> strfind(string search, string text)
{
 return (strpos(search, text) > 0);
}

function<string> strtrim_l(string text)
{
 while (isspace(text[1]))
  text = strcopy(text, 2, strlen(text));

 return text;
}

function<string> strtrim_r(string text)
{
 while (true)
 {
  var<int> i = strlen(text);
  if (isspace(text[i]))
   text = strcopy(text, 1, i-1); else
   break;
 }

 return text;
}

function<string> strtrim(string text)
{
 return strtrim_l(strtrim_r(text));
}

function<string> strremove_begin(string text, char ch)
{
 while (true)
 {
  if (text[1] == ch)
   text = strdelete(text, 1, 1); else
   break;
 }
 
 return text;
}

function<string> strremove_end(string text, char ch)
{
 while (true)
 {
  var<int> i = strlen(text);
  if (text[i] == ch)
   text = strdelete(text, i, 1); else
   break;
 }
 
 return text;
}

function<string> strupper(string text)
{
 for (var<int> i=1; i<=strlen(text); i++)
  text[i] = chupper(text[i]);
  
 return text;
}

function<string> strlower(string text)
{
 for (var<int> i=1; i<=strlen(text); i++)
  text[i] = chlower(text[i]);
  
 return text;
}

function<string> strdup(string text, int amount)
{
 var<string> result = "";
 
 for (var<int> i=0; i<amount; i++)
  result += text;
  
 return result;
}

function<bool> strstarts(string text, string str)
{
 var<int> len = strlen(str);
 var<int> len2 = strlen(text);
 
 if (len > len2)
  return false;
  
 if (len == len2)
  return text==str;
  
 for (var<int> i=1; i<=len; i++)
  if (text[i] != str[i])
   return false; 
 
 return true;
}

function<bool> strends(string text, string str)
{
 var<int> len = strlen(str);
 var<int> len2 = strlen(text);
 
 if (len > len2)
  return false;
  
 if (len == len2)
  return text==str; 

 for (var<int> i=0; i<len; i++)
  if (text[len2-i] != str[len-i])
   return false;
  
 return true;
}

function<string> strinsert(string text, string what, int at)
{
 var<int> len = strlen(what);
 var<int> len2 = strlen(text);
 
 if (at > len2) // too far
  at = len2;
  
 if (at == len2) // a little optimization ;>
  return text+what;

 return strcopy(text, 1, at-1)+
        what+
	strcopy(text, at, len2);
}

function<string> strreplace_ex(string text, string search, string replace, bool only_once)
{
 while (true)
 {
  var<int> pos = strpos(search, text);
  var<int> slen = strlen(search);
  
  if (pos == 0) // `search` not found
   return text;
   
  text = strdelete(text, pos, slen);
  text = strinsert(text, replace, pos);
  
  if (only_once) 
   return text;
 }
}

function<string> strreplace(string text, string search, string replace)
{
 return strreplace_ex(text, search, replace, false);
}

function<string> strreplace_once(string text, string search, string replace)
{
 return strreplace_ex(text, search, replace, true);
}

function<string> strleft(string text, int count)
{
 var<string> str = "";

 if (count <= 0)
  return "";

 if (count > strlen(text))
  count = strlen(text);

 for (var<int> i=1; i<=count; i++)
  str += text[i];

 return str;
}

function<string> strright(string text, int count)
{
 var<int> len = strlen(text);
 var<string> str = "";

 if (count <= 0)
  return "";

 if (count > len)
  count = len;

 for (var<int> i=len-count+1; i<=len; i++)
  str += text[i];

 return str;
}

function<bool> strcmp(string c1, string c2)
{
 return c1 == c2;
}

function<bool> strcmpl(string c1, string c2)
{
 var<int> len1 = strlen(c1);
 var<int> len2 = strlen(c2);

 if (len1 != len2)
  return false;

 for (var<int> i=1; i<=len1; i++)
  if (chlower(c1[i]) != chlower(c2[i]))
   return false;
}

function<string> boolstr(bool b)
{
 if (b)
  return "true"; else
  return "false";
}

function<string> boolstrb(bool b, string _true, string _false)
{
 if (b)
  return _true; else
  return _false;
}

function<string[]> strexplode(string text, char sep)
{
 var<string[]> result = new string[strcount(text, sep)+1];
 var<int> result_pos = 0;

 var<string> current = "";
 var<int> len = strlen(text);

 for (var<int> i=1; i<=len; i++)
 {
  if (text[i] == sep)
  {
   result[result_pos++] = current;
   current = "";
  } else
   current += text[i];
 }

 result[result_pos] = current;

 return result;
}

function<string> strimplode(string[] arr, string separator)
{
 var<string> result = "";
 var<int> arlen = arr.length();

 for (var<int> i=0; i<arlen; i++)
  if (i == arlen-1)
   result += arr[i]; else
   result += arr[i]+separator;

 return result;
}

function<string> streach(string text, function<char>(char) func)
{
 var<int> len = strlen(text);

 for (var<int> i=1; i<=len; i++)
  text[i] = func(text[i]);

 return text;
}

function<string> streach_index(string text, function<char>(char, int) func)
{
 var<int> len = strlen(text);

 for (var<int> i=1; i<=len; i++)
  text[i] = func(text[i], i);

 return text;
}

function<string> streach_index_string(string text, function<char>(char, int, string) func)
{
 var<int> len = strlen(text);

 for (var<int> i=1; i<=len; i++)
  text[i] = func(text[i], i, text);

 return text;
}

function<char[]> stringarray(string text)
{
 var<int> len = strlen(text);
 var<char[]> arr = new char[len];

 for (var<int> i=0; i<len; i++)
  arr[i] = text[i+1];

 return arr;
}

function<string> arraystring(char[] text)
{
 var<string> str = "";

 for (var<int> i=0; i<text.length(); i++)
  str += text[i];

 return str;
}

}
