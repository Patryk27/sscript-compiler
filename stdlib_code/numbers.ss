/*
 SScript Standard Library
 numbers.ss
*/

@("string.ss")
@("math.ss")
@("vm.ss")
@visibility("public")

namespace std
{

function<string> intsys(int num, int base)
{
 if ((base < 2) || (base > 35))
  throwError("Invalid numeric base: "+intsys(base, 10));

 var<string> result="";
 var<bool> neg = (num<0);
 
 num = iabs(num);
 
 if (num == 0)
  return "0";
 
 while (num != 0)
 {             
  var<int> rem = num%base;
  if (rem > 9)
   result += cast<char>('A'-10+rem); else
   result += cast<char>(rem%10+48);
  num /= base;
 }
 
 if (neg)
  result += "-";
 
 return strreverse(result); 
}

function<int> sysint(string num, int base)
{
 if ((base < 2) || (base > 35))
  throwError("Invalid numeric base: "+intsys(base, 10));

 var<int> result=0, tmp=0;
 var<bool> neg = (num[1] == '-');
 
 if (neg)
  num = strdelete(num, 1, 1); // remove leading `-`
 
 num = strlower(num);

 if ((num == "") || (num == "0"))
  return 0;
  
 var<int> len = strlen(num);
 for (var<int> i=1; i<=len; i++)
 {
  var<char> ch = num[i];
  
  if (isalpha(ch))
   tmp = ch-'a'+10; else
   tmp = ch-'0';

  if (tmp > base) // invalid number
   throwError("Error converting number `"+num+"` to base "+intsys(base, 10));
   
  result += tmp*ipower(base, len-i);
 }
 
 if (neg)
  return -result; else
  return result;
}

function<int> intlen(int n)
{
 n = iabs(n);
 if (n == 0) // special case: log10(0) is -inf
  return 0;
 return round_up(log10(n));
}

function<string> intstr(int n)
{
 return intsys(n, 10);
}

function<int> strint(string str)
{
 return sysint(str, 10);
}

function<string> intbin(int n)
{
 return intsys(n, 2);
}

function<int> binint(string str)
{
 return sysint(str, 2);
}

function<string> inthex(int n)
{
 return intsys(n, 16);
}

function<int> hexint(string str)
{
 return sysint(str, 16);
}

function<string> fltstr(float n)                 
{
 var<string> result = "", tmp = "";
 var<bool> neg = (n < 0);
 var<int> mult = 1, idx = 0, wholeNum, decimalNum;

 n = fabs(n);
 
 for (idx=0; idx<8; idx++)
  mult *= 10;
 
 wholeNum = round_trunc(n);
 decimalNum = round((n-wholeNum)*mult);

 result = intstr(wholeNum);
 
 if (decimalNum > 0)
 {
  result += '.';
  idx = 0;
  while (decimalNum != 0)
  {
   tmp += cast<char>('0'+(decimalNum%10));
   decimalNum /= 10;
   idx++;
  }
  while (idx < 8)
  {
   tmp += '0';
   idx++;
  }
  
  result += strreverse(tmp);
 }

 if (neg)
  result = '-'+result;
 
 if (strposb(".", result))
  return strremove_end(result, '0'); /* remove ending zeroes */ else
  return result;
}

function<float> strflt(string str)
{
 var<string> wholePart="", decimalPart=" ";
 var<float> left=0, right=0;
 var<int> dot = strpos(".", str);

 if (dot > 0)
 {
  wholePart = strcopy(str, 1, dot-1);
  decimalPart = strtrim(strcopy(str, dot+1, strlen(str)));
 } else
  wholePart = str;

 left = strint(wholePart);
 
 if (decimalPart != " ")
  right = strint(decimalPart)/power(10, strlen(decimalPart));

 return left+right;
}

}