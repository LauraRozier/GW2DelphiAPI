unit GW2DA_Misc;

interface
uses
  // System units
  Sysutils, Classes, REST.JSON, JSON,
  // GW2 Delphi API Units
  GW2DA_Defaults, GW2DA_Types, GW2DA_WebHandlers;

type
  TGW2APIMisc = class(TObject)
    public
      constructor Create;
      procedure GetBuild(aWebHandler: TWebHandler; aAPIVersion: TAPIVersion;
                         aVersion: TGW2Version);
      procedure GetQuagganIDs(aWebHandler: TWebHandler; aStringList: TStringList);
      function GetColorIDs(aWebHandler: TWebHandler): TIntegerArray;
      function GetColors(aWebHandler: TWebHandler; aParams: TUrlParams): TGW2ColorList;
  end;

implementation

constructor TGW2APIMisc.Create;
begin
  Inherited Create;
end;


procedure TGW2APIMisc.GetBuild(aWebHandler: TWebHandler; aAPIVersion: TAPIVersion;
                               aVersion: TGW2Version);
var
  Reply:   string;
  JSObject: TJSONObject;
begin
  case aAPIVersion of
    APINone:
      raise Exception.Create('Unsupported API version.');
    APIv1:
    begin
      Reply       := aWebHandler.FetchEndpoint(APIv1, v1Build, nil);
      JSObject    := TJSONObject.ParseJSONValue(Reply) as TJSONObject;
      aVersion.id := JSObject.GetValue<Integer>('build_id'); // Works
    end;
    APIv2:
    begin
      Reply       := aWebHandler.FetchEndpoint(APIv2, v2Build, nil);
      JSObject    := TJSONObject.ParseJSONValue(Reply) as TJSONObject;
      aVersion.id := JSObject.GetValue<Integer>('id'); // Works
    end;
  end;

  // Both of these fail... They return 0
  // FetchEndpoint returns the raw JSON string, eg: {"id":66577}
  //
  // aVersion := TJson.JsonToObject<TGW2Version>(IResult);
  // aVersion := TJson.JsonToObject<TGW2Version>(IJSONObject);
end;


procedure TGW2APIMisc.GetQuagganIDs(aWebHandler: TWebHandler; aStringList: TStringList);
var
  Reply:   string;
  JSArr:   TJSONArray;
  JSValue: TJSONValue;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Quaggans, nil);
  JSArr  := TJSONObject.ParseJSONValue(Reply) as TJSONArray;

  for JSValue in JSArr do
    aStringList.Add(JSValue.Value);
end;


function TGW2APIMisc.GetColorIDs(aWebHandler: TWebHandler): TIntegerArray;
var
  Reply:   string;
  JSArr:   TJSONArray;
  IntArr:  TIntegerArray;
  I:       Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Colors, nil);
  JSArr  := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(IntArr, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
    IntArr[I] := StrToInt(JSArr.Items[I].Value);

  Result := IntArr;
end;


function TGW2APIMisc.GetColors(aWebHandler: TWebHandler; aParams: TUrlParams): TGW2ColorList;
var
  Reply:     string;
  JSArr:     TJSONArray;
  JSObject:  TJSONObject;
  I:         Integer;
  ColorList: TGW2ColorList;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Colors, aParams);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(ColorList, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
  begin
    JSObject     := JSArr.Items[I] as TJSONObject;
    ColorList[I] := TJson.JsonToObject<TGW2Color>(JSObject);
  end;

  Result := ColorList;
end;

end.
