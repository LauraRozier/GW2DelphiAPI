unit GW2DA_Utils;

interface
uses
  SysUtils, TypInfo, SysConst;
         {
type
  TEnumHlpr<TEnum> = class
    public
      class function EnumFromString(const str: string): TEnum;
  end;   }

implementation
        {
class function TEnumHlpr<TEnum>.EnumFromString(const str: string): TEnum;
var
  typeInf: PTypeInfo;
begin
  typeInf := PTypeInfo(TypeInfo(TEnum));
    if typeInf^.Kind <> tkEnumeration then
      raise EInvalidCast.CreateRes(@SInvalidCast);

  for Result := low(Result) to high(Result) do // Fails here, needs work
    if TEnum[Result] = str then
      exit;

  raise Exception.CreateFmt('Enum %s not found', [str]);
end;   }

end.
