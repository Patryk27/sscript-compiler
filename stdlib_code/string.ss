/*
 SScript Standard Library
 string.ss
*/

public;

namespace std
{
 /* ===== char ===== */
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

 /* ===== string ===== */
 function<int> strcount(string text, char sep)
 {
  var<int> count = 0, len = text.length();

  for (var<int> i=1; i<=len; i++)
   if (text[i] == sep)
    count++;

  return count;
 }

 function<string> strreverse(string text)
 {
  var<string> str = "";
  
  for (var<int> i=text.length(); i>=1; i--)
   str += text[i];
   
  return str;
 }

 function<string> strcopy(string text, int begin, int length)
 {
  if (begin < 0) // negative `begin`
   begin = text.length()+begin+1;

  if ((begin < 0) || (length < 0)) // invalid length
   return "";

  var<int> end = begin+length-1;

  if (end > text.length()) // too long
   end = text.length();

  var<string> result = "";
  for (var<int> i=begin; i<=end; i++) // copy string char-by-char
   result += text[i];

  return result;
 }

 function<string> strdelete(string text, int begin, int length)
 {
  if (begin < 0)
   return strdelete(text, text.length()+begin+1, length);

  return strcopy(text, 1, begin-1)+
         strcopy(text, begin+length, text.length());
 }

 function<string> strdelete_begin(string text, int count)
 {
  return strcopy(text, count, text.length());
 }

 function<string> strdelete_end(string text, int count)
 {
  return strdelete(text, text.length()-count, count);
 }

 function<int> strpos(string search, string text, int begin=1)
 {                   
  var<int> index = 0;
  var<int> len = text.length();
  var<int> slen = search.length();
        
  if ((slen > len) || (slen == 0))
   return 0;
        
  if ((slen == len) && (search == text))
   return 1;

  for (var<int> i=begin; i<=len; i++)
  {
   if (text[i] == search[1])
   {
    var<bool> found=true;

    for (var<int> q=1; q<=slen; q++)
    {
     if (text[i+q-1] != search[q]) 
     { 
      found = false;
      break;
     }
    }
     
    if (found)
    {
     index = i;
     break;
    }
   }
  }

  return index;
 }

 function<bool> strfind(string search, string text)
 {
  return (strpos(search, text) > 0);
 }

 function<string> strtrim_l(string text)
 {
  while (isspace(text[1]))
  {
   text = strcopy(text, 2, text.length());

   if (text.length() == 0)
    return "";
  }

  return text;
 }

 function<string> strtrim_r(string text)
 {
  while (true)
  {
   var<int> i = text.length();
 
   if (i == 0)
    return "";

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
   if (text.length() == 0)
    return "";

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
   var<int> i = text.length();

   if (i == 0)
    return "";
 
   if (text[i] == ch)
    text = strdelete(text, i, 1); else
    break;
  }
 
  return text;
 }

 function<string> strupper(string text)
 {
  var<int> len = text.length();
  for (var<int> i=1; i<=len; i++)
   text[i] = chupper(text[i]);

  return text;
 }

 function<string> strlower(string text)
 {
  var<int> len = text.length();
 
  for (var<int> i=1; i<=len; i++)
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
  var<int> len = str.length();
  var<int> len2 = text.length();
  
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
  var<int> len = str.length();
  var<int> len2 = text.length();
  
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
  var<int> len = what.length();
  var<int> len2 = text.length();
  
  if (at > len2) // too far
   at = len2;
   
  if (at == len2) 
   return text+what;

  return strcopy(text, 1, at-1)+
         what+
         strcopy(text, at, len2);
 }

 function<string> strreplace(string text, string search, string replace, bool only_once=false)
 {
  while (true)
  {
   var<int> pos = strpos(search, text);
   var<int> slen = search.length();
  
   if (pos == 0) // `search` not found
    return text;
   
   text = strdelete(text, pos, slen);
   text = strinsert(text, replace, pos);
  
   if (only_once) 
    return text;
  }
 }

 function<string> strleft(string text, int count)
 {
  var<string> str = "";

  if (count <= 0)
   return "";

  if (count > text.length())
   count = text.length();

  for (var<int> i=1; i<=count; i++)
   str += text[i];

  return str;
 }

 function<string> strright(string text, int count)
 {
  var<int> len = text.length();
  var<string> str = "";

  if (count <= 0)
   return "";

  if (count > len)
   count = len;

  for (var<int> i=len-count+1; i<=len; i++)
   str += text[i];

  return str;
 }

 function<bool> strcmp(string c1, string c2, bool case_sensitive=true)
 {
  if (case_sensitive)
  {
   return (c1 == c2);
  } else
  {
   var<int> len1 = c1.length();
   var<int> len2 = c2.length();

   if (len1 != len2)
    return false;

   for (var<int> i=1; i<=len1; i++)
    if (chlower(c1[i]) != chlower(c2[i]))
     return false;

   return true;
  }
 }

 function<string> boolstr(bool b, string on_true="true", string on_false="false")
 {
  if (b)
   return on_true; else
   return on_false;
 }

 function<string[]> strexplode(string text, char sep)
 {
  var<string[]> result = new string[strcount(text, sep)+1];
  var<int> result_pos = 0;

  var<string> current = "";
  var<int> len = text.length();

  for (var<int> i=1; i<=len; i++)
  {
   if (text[i] == sep)
   {
    result[result_pos++] = current;
    current = "";
   } else
   {
    current += text[i];
   }
  }

  result[result_pos] = current;

  return result;
 }

 function<string> strimplode(string[] arr, string separator)
 {
  var<string> result = "";
  var<int> arlen = arr.length();

  for (var<int> i=0; i<arlen; i++)
  {
   if (i == arlen-1)
    result += arr[i]; else
    result += arr[i]+separator;
  }

  return result;
 }

 function<string> streach(string text, function<char>(char) func)
 {
  var<int> len = text.length();

  for (var<int> i=1; i<=len; i++)
   text[i] = func(text[i]);

  return text;
 } 

 function<char[]> stringarray(string text)
 {
  var<int> len = text.length();
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