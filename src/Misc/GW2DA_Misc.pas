unit GW2DA_Misc;

interface
uses
  // System units
  Sysutils, Classes, REST.JSON, JSON,
  // GW2 Delphi API Units
  GW2DA_Defaults, GW2DA_Types, GW2DA_WebHandlers;

type
  TGW2APIMisc = class(TObject)
    private
    public
      constructor Create;
      procedure GetBuild(aWebHandler: TWebHandler; aVersion: TGW2Version);
      procedure GetQuaggans(aWebHandler: TWebHandler; aStringList: TStringList); //: TIDStringArray;
  end;

implementation

constructor TGW2APIMisc.Create;
begin
  Inherited Create;
end;


procedure TGW2APIMisc.GetBuild(aWebHandler: TWebHandler; aVersion: TGW2Version);
var
  IResult:     string;
  IJSONObject: TJSONObject;
begin
  IResult     := aWebHandler.FetchEndpoint(APIv2, v2Build, nil);
  IJSONObject := TJSONObject.ParseJSONValue(IResult) as TJSONObject;

  // Both of these fail... They return 0
  // FetchEndpoint returns the raw JSON string, eg: {"id":66577}
  //
  //   aVersion := TJson.JsonToObject<TGW2Version>(IResult);
  //   aVersion := TJson.JsonToObject<TGW2Version>(IJSONObject);
  //

  aVersion.id := IJSONObject.GetValue<Integer>('id'); // Works
end;


procedure TGW2APIMisc.GetQuaggans(aWebHandler: TWebHandler; aStringList: TStringList); //: TIDStringArray;
var
  IResult:    string;
  IJSONArr:   TJSONArray;
  IJSONValue: TJSONValue;
begin
  IResult  := aWebHandler.FetchEndpoint(APIv2, v2Quaggans, nil);
  IJSONArr := TJSONObject.ParseJSONValue(IResult) as TJSONArray;

  for IJSONValue in IJSONArr do
    aStringList.Add(IJSONValue.Value);
end;

end.
