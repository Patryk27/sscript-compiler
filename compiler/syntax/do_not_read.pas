(*
 Good luck in reversing this file! ;)
 (but I'm not responsible for any damage)
*)

{$MACRO ON}{$DEFINE o:=begin}{$DEFINE k:=end}{$DEFINE t:=qword}{$DEFINE x:=pointer}{$DEFINE v:=var}{$DEFINE m:=mod}{$DEFINE d:=inc}{$DEFINE g:=forward}{$DEFINE c:=const}{$DEFINE p:=procedure}
p&v;g;p&m;o;&v();k;c magic1=%00001101111011111010110011101101;magic2=%11011110101011011100000011011110;magic:array[%1010..%10100001010]of t=(
$37BEB3BAE,$37BEB3BA1,$37BEB3BAE,$37BEB3B461,$DEFACF3E,$37BEB415,$DEFAD5B,$37BEB415,$DEFACF3E,$37BEB3B461,$37BEB3BAE,
$37BEB3BA1,$37BEB3B6C,$37BEB3B420,$DEFACEDD,$37BEB3BE,$DEFAD5B,$37BEB415,$DEFACF3E,$37BEB3B461,$37BEB3BAE,$37BEB3BA1,$37BEB3BAE,
$37BEB3B461,$DEFACF3E,$37BEB415,$DEFAD5B,$37BEB415,$DEFACEFC,$37BEB3B40D,$37BEB3B4A,$37BEB3B82,$37BEB3B81,$37BEB3B454,$DEFACF1D,
$37BEB3F5,$DEFAD3B,$37BEB3D5,$DEFACEDD,$37BEB3B40A,$37BEB3B49,$37BEB3B49,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD19,
$37BEB3E2,$DEFACF3F,$37BEB3B46F,$37BEB3BA4,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,
$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B462,$DEFACF3F,$37BEB423,$DEFAD1B,$37BEB3E0,$DEFACEDD,$37BEB3B40A,$37BEB3B60,
$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,
$37BEB3B420,$DEFACEF0,$37BEB3E2,$DEFAD5C,$37BEB418,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B90,$37BEB3B9E,$37BEB3B422,$DEFACEF2,
$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,
$37BEB3D6,$DEFACEF2,$37BEB3B45E,$37BEB3B99,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF32,$37BEB423,$DEFAD1B,$37BEB3C1,$DEFACEDA,
$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B6E,
$37BEB3BAF,$37BEB3BA4,$37BEB3B438,$DEFACF20,$37BEB3DB,$DEFAD14,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B6E,$37BEB3B6E,$37BEB3BAF,
$37BEB3B46F,$DEFACF3F,$37BEB418,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF32,
$37BEB423,$DEFAD5C,$37BEB423,$DEFACEFE,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B460,$DEFACF30,$37BEB40D,$DEFAD25,
$37BEB416,$DEFACF3F,$37BEB3B42E,$37BEB3B4D,$37BEB3B4A,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,
$37BEB3B420,$37BEB3B6E,$37BEB3BAF,$37BEB3BA4,$37BEB3B450,$DEFACEF7,$37BEB3D6,$DEFAD0D,$37BEB3D4,$DEFACEFE,$37BEB3B46F,$37BEB3BAF,
$37BEB3BA4,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,
$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF32,
$37BEB423,$DEFAD5C,$37BEB3E2,$DEFACEF0,$37BEB3B420,$37BEB3B62,$37BEB3BA0,$37BEB3B99,$37BEB3B462,$DEFACF3F,$37BEB3E2,$DEFACFA,
$37BEB3BE,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEFE,$37BEB418,$DEFAD25,$37BEB3DB,$DEFACEF0,
$37BEB3B420,$37BEB3B60,$37BEB3BAF,$37BEB3BA4,$37BEB3B438,$DEFACEF7,$37BEB418,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,
$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB41A,$DEFAD4D,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B67,$37BEB3BB4,
$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3BA2,$37BEB3B460,$DEFACF08,
$37BEB416,$DEFAD5C,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3BA0,$37BEB3B99,$37BEB3BA2,$37BEB3B42E,$DEFACEDD,$37BEB3BE,$DEFAD0D,
$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3BA4,$37BEB3B78,$37BEB3B427,$DEFACEF0,$37BEB3D4,$DEFAD5C,$37BEB418,$DEFACF08,
$37BEB3B45E,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,
$37BEB3B78,$37BEB3B78,$37BEB3B45B,$DEFACEF0,$37BEB3D4,$DEFAD4D,$37BEB3DB,$DEFACEF0,$37BEB3B420,$37BEB3B9D,$37BEB3B78,$37BEB3B78,
$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACF2E,
$37BEB3EC,$DEFAD4F,$37BEB423,$DEFACEF0,$37BEB3B420,$37BEB3BA0,$37BEB3B78,$37BEB3BA2,$37BEB3B40D,$DEFACEDA,$37BEB3D4,$DEFAD0D,
$37BEB3D4,$DEFACEFE,$37BEB3B438,$37BEB3B90,$37BEB3B60,$37BEB3B60,$37BEB3B464,$DEFACF08,$37BEB3EC,$DEFAD14,$37BEB3D4,$DEFACEF0,
$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,
$37BEB3B78,$37BEB3B90,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B459,$37BEB3B78,$37BEB3B78,$37BEB3B78,
$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,
$37BEB414,$DEFAD25,$37BEB3EC,$DEFACF32,$37BEB3B420,$37BEB3B60,$37BEB3B99,$37BEB3B78,$37BEB3B42E,$DEFACEDD,$37BEB3BE,$DEFAD0D,
$37BEB3D4,$DEFACF34,$37BEB3B438,$37BEB3B67,$37BEB3B60,$37BEB3B6E,$37BEB3B464,$DEFACF08,$37BEB3DB,$DEFAD0D,$37BEB3D4,$DEFACEF0,
$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B460,$DEFACF29,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,
$37BEB3B78,$37BEB3B78,$37BEB3B427,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3BA0,$37BEB3B78,$37BEB3B78,
$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B450,$37BEB3B67,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,
$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACF30,$37BEB3B438,$37BEB3BA2,$37BEB3B6E,$37BEB3B60,$37BEB3B460,$DEFACF08,$37BEB416,$DEFACFA,
$37BEB3BE,$DEFACEF0,$37BEB3B42E,$37BEB3B78,$37BEB3B90,$37BEB3B60,$37BEB3B42E,$DEFACF08,$37BEB3EC,$DEFAD3D,$37BEB3D4,$DEFACEF0,
$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B422,$37BEB3B62,
$37BEB3B62,$37BEB3B62,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,
$37BEB3B420,$DEFACEF0,$37BEB3D6,$DEFAD0F,$37BEB3D6,$DEFACEF2,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,
$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B99,$37BEB3B78,$37BEB3B438,$DEFACEFE,$37BEB3D4,$DEFAD46,
$37BEB3EC,$DEFACEFE,$37BEB3B40D,$37BEB3B4A,$37BEB3B60,$37BEB3B78,$37BEB3B438,$DEFACEF0,$37BEB3D4,$DEFAD25,$37BEB3EC,$DEFACF08,
$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,
$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,
$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,
$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3EC,$DEFAD25,
$37BEB3EC,$DEFACEF0,$37BEB3B420,$37BEB3B78,$37BEB3B78,$37BEB3B4D,$37BEB3B40A,$DEFACEF0,$37BEB3EC,$DEFAD25,$37BEB3D4,$DEFACEF0,
$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,
$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,
$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,
$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,
$37BEB3D4,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B60,$37BEB3B60,$37BEB3B438,$DEFACF08,$37BEB3C1,$DEFACF7,$37BEB3D4,$DEFACF08,
$37BEB3B438,$37BEB3B60,$37BEB3B60,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB3E2,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,
$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEFE,$37BEB3E2,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,
$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,
$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B6E,$37BEB3B6E,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,
$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B6E,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACEF0,$37BEB3D4,$DEFAD25,$37BEB3EC,$DEFACEDD,
$37BEB3B40A,$37BEB3B60,$37BEB3BA0,$37BEB3B78,$37BEB3B462,$DEFACEF0,$37BEB414,$DEFAD25,$37BEB3EC,$DEFACF32,$37BEB3B42C,$37BEB3B60,
$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB418,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3BA2,$37BEB3B6E,$37BEB3BAF,
$37BEB3B464,$DEFACF08,$37BEB416,$DEFAD5C,$37BEB3E2,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEFE,
$37BEB423,$DEFAD51,$37BEB3EC,$DEFACF32,$37BEB3B46F,$37BEB3B6E,$37BEB3BA4,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD4F,
$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B6C,$37BEB3B464,$DEFACF08,$37BEB3EC,$DEFAD14,$37BEB3D4,$DEFACF34,
$37BEB3B438,$37BEB3B67,$37BEB3B4D,$37BEB3B4A,$37BEB3B420,$DEFACEF0,$37BEB40D,$DEFAD25,$37BEB3E2,$DEFACEF0,$37BEB3B460,$37BEB3B99,
$37BEB3B78,$37BEB3B78,$37BEB3B42E,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,
$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3BA2,$37BEB3B60,$37BEB3B420,$DEFACEF0,
$37BEB3D4,$DEFAD51,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,
$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEFE,$37BEB3EC,$DEFAD25,$37BEB404,$DEFACEF7,
$37BEB3B420,$37BEB3B6E,$37BEB3B78,$37BEB3B90,$37BEB3B40D,$DEFACEDA,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACF30,$37BEB3B438,$37BEB3BA2,
$37BEB3B60,$37BEB3B60,$37BEB3B459,$DEFACF08,$37BEB3EC,$DEFAD4F,$37BEB3E2,$DEFACEF0,$37BEB3B420,$37BEB3BA0,$37BEB3B78,$37BEB3B78,
$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,
$37BEB3D4,$DEFAD0D,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,
$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B67,$37BEB3B60,$37BEB3B420,$DEFACEFE,$37BEB418,$DEFAD25,$37BEB3EC,$DEFACF20,
$37BEB3B420,$37BEB3B60,$37BEB3BA4,$37BEB3B78,$37BEB3B427,$DEFACEDD,$37BEB3BE,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,
$37BEB3B99,$37BEB3B78,$37BEB3B42E,$DEFACEF0,$37BEB3D4,$DEFAD4B,$37BEB40D,$DEFACF08,$37BEB3B438,$37BEB3BA2,$37BEB3BAF,$37BEB3BA4,
$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,
$37BEB3EC,$DEFAD1B,$37BEB3E2,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,
$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3BA2,$37BEB3BAF,$37BEB3B464,$DEFACF08,$37BEB3EC,$DEFAD3D,$37BEB412,$DEFACEF0,
$37BEB3B420,$37BEB3B6E,$37BEB3B78,$37BEB3B90,$37BEB3B40D,$DEFACEDA,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,
$37BEB3BA0,$37BEB3B99,$37BEB3B438,$DEFACEFE,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACF2E,$37BEB3B459,$37BEB3B78,$37BEB3B78,$37BEB3B78,
$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,
$37BEB400,$DEFAD40,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,
$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B90,$37BEB3B45E,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3E2,$DEFACF08,
$37BEB3B450,$37BEB3B67,$37BEB3B4D,$37BEB3B4A,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,
$37BEB3BA0,$37BEB3B9E,$37BEB3B459,$DEFACF32,$37BEB3E2,$DEFAD19,$37BEB3D4,$DEFACEF0,$37BEB3B460,$37BEB3B9E,$37BEB3B9E,$37BEB3B99,
$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,
$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB404,$DEFAD4B,
$37BEB412,$DEFACEF7,$37BEB3B420,$37BEB3B60,$37BEB3B6C,$37BEB3B6E,$37BEB3B464,$DEFACF20,$37BEB412,$DEFAD14,$37BEB3C1,$DEFACEDA,
$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3BA0,
$37BEB3B9E,$37BEB3B99,$37BEB3B438,$DEFACF32,$37BEB3E2,$DEFAD1B,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3BA0,$37BEB3BA0,$37BEB3B9E,
$37BEB3B45E,$DEFACF2E,$37BEB40D,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF20,
$37BEB412,$DEFAD4B,$37BEB412,$DEFACEF7,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B42E,$DEFACEFE,$37BEB418,$DEFAD25,
$37BEB404,$DEFACF2E,$37BEB3B427,$37BEB3B4D,$37BEB3B4A,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,
$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB414,$DEFACF2E,$37BEB3B459,$37BEB3B78,
$37BEB3B78,$37BEB3B78,$37BEB3B462,$DEFACF3F,$37BEB3E2,$DEFAD19,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,
$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B42C,$37BEB3B6E,$37BEB3BAF,$37BEB3BA4,$37BEB3B438,$DEFACF08,
$37BEB3EC,$DEFAD3D,$37BEB412,$DEFACEF7,$37BEB3B40D,$37BEB3B4A,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,
$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B60,$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,
$37BEB3B420,$37BEB3B60,$37BEB3B62,$37BEB3BA0,$37BEB3B45E,$DEFACF2E,$37BEB40D,$DEFAD25,$37BEB3EC,$DEFACF08,$37BEB3B438,$37BEB3B78,
$37BEB3B78,$37BEB3B78,$37BEB3B438,$DEFACF08,$37BEB3EC,$DEFAD25,$37BEB3EC,$DEFACF20,$37BEB3B45E,$37BEB3B9E,$37BEB3B67,$37BEB3B62,
$37BEB3B420,$DEFACEF0,$37BEB3D4,$DEFAD0D,$37BEB3D4,$DEFACEF0,$37BEB3B420,$37BEB3B60,$37BEB3B60,$37BEB3B8C,$37BEB3B453,$DEFACEDD,
$37BEB3BE,$DEFACFA,$37BEB3BE,$DEFACF24,$37BEB3B468,$37BEB3BA1,$37BEB3BAE,$37BEB3BAB,$37BEB3B473,$DEFACEF0,$37BEB428,$DEFAD5C,
$37BEB3D4,$DEFACF38,$37BEB3B474,$37BEB3BB4,$37BEB3BB0,$37BEB3B7A,$37BEB3B42F,$DEFACEFF,$37BEB42B,$DEFAD64,$37BEB42B,$DEFACEFE,
$37BEB3B463,$37BEB3BA8,$37BEB3BB2,$37BEB3BA9,$37BEB3B473,$DEFACEFE,$37BEB417,$DEFAD5C,$37BEB421,$DEFACEFF,$37BEB3B461,$37BEB3BB3,
$37BEB3BA3,$37BEB3BA9,$37BEB3B469,$DEFACEFF,$37BEB3D4,$DEFAD27,$37BEB3DD,$DEFACEDD,$37BEB3B40A,0);
p&v;v a:x=x(2);i:t=magic1;b:t=magic2;o;a:=x(a+t(@magic));while(pbyte(a-(b-magic2))^<>0)do o;write(chr(pqword(a-2-(b-magic2))^-(i*(1+b m 2)<<((B*B)m 10))));d(b);d(a,sizeof(t)+1);k;writeln;k;v&const:p=@&m;