unit GW2DA_Authentication;

interface
uses
  // System units
  SysUtils, REST.JSON, JSON,
  // GW2 Delphi API Units
  GW2DA_Types, GW2DA_WebHandlers;

function GW2TokenInfo(aWebHandler: TWebHandler; aAuthStr: string): TGW2Token;

implementation

function GW2TokenInfo(aWebHandler: TWebHandler; aAuthStr: string): TGW2Token;
var
  Reply:     string;
  JSObject:  TJSONObject;
  AuthToken: TGW2Token;
begin
  Reply     := aWebHandler.FetchAuthEndpoint(APIv2, v2Tokeninfo, nil, aAuthStr);
  JSObject  := TJSONObject.ParseJSONValue(Reply) as TJSONObject;
  AuthToken := TJson.JsonToObject<TGW2Token>(JSObject);
  Result    := AuthToken;
end;

end.
