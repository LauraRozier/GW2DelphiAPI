unit GW2DelphiAPI;

interface
{$I GW2DelphiAPI.inc}

implementation

{ API data types }
// TGW2ColorDetail
function TGW2ColorDetail.GetRGB(aIndex: Integer): Integer;
begin
  Result := fRGB[aIndex];
end;


procedure TGW2ColorDetail.SetRGB(aIndex, aValue: Integer);
begin
  fRGB[aIndex] := aValue;
end;


// TGW2Color
function TGW2Color.GetRGB(aIndex: Integer): Integer;
begin
  Result := fBase_RGB[aIndex];
end;


procedure TGW2Color.SetRGB(aIndex, aValue: Integer);
begin
  fBase_RGB[aIndex] := aValue;
end;


{ Web handler class }
constructor TWebHandler.Create;
begin
  Inherited Create;
end;


function TWebHandler.SendRequest(aUrl: string): TErrorMessage;
var
  Response: string;
  Error:    TErrorMessage;
begin
   Error.HadError := False;
   Error.Msg := '';

  try
    Response := fHTTPClient.Get(aUrl);
  except
    // Indy protocol exception
    on E:EIdHTTPProtocolException do
    begin
      Error.Msg := 'Error: Indy raised a protocol error!'       + sLineBreak +
                   'HTTP status code: ' + IntToStr(E.ErrorCode) + sLineBreak +
                   'Error message'      + E.Message             + sLineBreak;
      Error.HadError := True;
    end;
    // Indy SSL Library exception
    on E:EIdOSSLCouldNotLoadSSLLibrary do
    begin
      Error.Msg := 'Error: Indy could not load SSL library!' + sLineBreak +
                   'Exception class: ' + E.ClassName                                + sLineBreak +
                   'Error message: '   + E.Message                                  + sLineBreak;
      Error.HadError := True;
    end;
    // Indy server closed connection exception
    on E:EIdConnClosedGracefully do
    begin
      Error.Msg := 'Error: Indy reports, that connection was closed by the server!' + sLineBreak +
                   'Exception class: ' + E.ClassName                                + sLineBreak +
                   'Error message: '   + E.Message                                  + sLineBreak;
      Error.HadError := True;
    end;
    // Indy low-level socket exception
    on E:EIdSocketError do
    begin
      Error.Msg := 'Error: Indy raised a socket error!'    + sLineBreak +
                   'Error code: '  + IntToStr(E.LastError) + sLineBreak +
                   'Error message' + E.Message             + sLineBreak;
      Error.HadError := True;
    end;
    // Indy read-timeout exception
    on E:EIdReadTimeout do
    begin
      Error.Msg := 'Error: Indy raised a read-timeout error!' + sLineBreak +
                   'Exception class: ' + E.ClassName          + sLineBreak +
                   'Error message: '   + E.Message            + sLineBreak;
      Error.HadError := True;
    end;
    // All other Indy exceptions
    on E:EIdException do
    begin
      Error.Msg := 'Error: Something went wrong with Indy!' + sLineBreak +
                   'Exception class: ' + E.ClassName        + sLineBreak +
                   'Error message: '   + E.Message          + sLineBreak;
      Error.HadError := True;
    end;
    // All other Delphi exceptions
    on E:Exception do
    begin
      Error.Msg := 'Error: Something non-Indy related raised an exception!' + sLineBreak +
                   'Exception class: ' + E.ClassName                        + sLineBreak +
                   'Error message: '   + E.Message                          + sLineBreak;
      Error.HadError := True;
    end;
  end;

  if not Error.HadError then
    Error.Msg := Response;

  Result := Error;
end;


function TWebHandler.BuildParamString(aParams: TUrlParams): string;
var
  Param:     TUrlParam;
  ResultStr: string;
begin
  ResultStr := '';

  for Param in aParams do
  begin
    if ResultStr = '' then
      ResultStr := '?' + Param.Name + '=' + Param.Value
    else
      ResultStr := ResultStr + '&' + Param.Name + '=' + Param.Value;
  end;

  Result := ResultStr;
end;


function TWebHandler.FetchRawEndpoint(aUrl: string): string;
begin
   Result := SendRequest(aUrl).Msg;
end;


function TWebHandler.FetchEndpoint(aVersion:  TAPIVersion;
                                   aFunction: TAPIFunction;
                                   aParams:   TUrlParams): string;
var
  Url:      string;
  Response: TErrorMessage;
begin
  Url := CONST_API_URL_BASE + CONST_API_Versions[aVersion] + '/' + CONST_API_Functions[aFunction];

  if (Length(aParams) > 0) and not (aParams = nil) then
    Url := Url + BuildParamString(aParams);

  Response := SendRequest(Url);

  if Response.HadError then
    raise Exception.Create('Something went wrong with this request!');

  Result := Response.Msg;
end;


function TWebHandler.FetchAuthEndpoint(aVersion:     TAPIVersion;
                                       aFunction:    TAPIFunction;
                                       aParams:      TUrlParams;
                                       aAuthString:  string
                                       {aPermissions: TAPIPermissions}): string;
begin
  //TODO 2 -oThimo -cWeb: Add code to determine if API key has the correct permissions
  if aAuthString = '' then
    raise Exception.Create('This API function requires authentication.');

  SetLength(aParams, Length(aParams) + 1);
  aParams[Length(aParams) - 1].Name  := 'access_token';
  aParams[Length(aParams) - 1].Value := aAuthString;

  Result := FetchEndpoint(aVersion, aFunction, aParams);
end;


{ Utilities }
class function TGW2Helper.StringToEnum<TEnum>(const aString: string): TEnum;
var
  TypeInf: PTypeInfo;
  Value:   Integer;
  PValue:  Pointer;
begin
  typeInf := PTypeInfo(TypeInfo(TEnum));
    if typeInf^.Kind <> tkEnumeration then
      raise EInvalidCast.CreateRes(@SInvalidCast);

  Value  := GetEnumValue(TypeInfo(TEnum), aString);

  if Value = -1 then
    raise Exception.CreateFmt('Enum %s not found', [aString]);

  PValue := @Value;
  Result := TEnum(PValue^);
end;


class function TGW2Helper.EnumToInt<TEnum>(const EnumValue: TEnum): Integer;
begin
  Result := 0;
  Move(EnumValue, Result, sizeOf(EnumValue));
end;


class function TGW2Helper.EnumToString<TEnum>(EnumValue: TEnum): string;
begin
  Result := GetEnumName(TypeInfo(TEnum), EnumToInt(EnumValue));
end;


function TGW2Helper.GW2TokenInfo(aWebHandler: TWebHandler; aAuthStr: string): TGW2Token;
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


{ API Misc functions class }
constructor TGW2APIMisc.Create;
begin
  Inherited Create;
end;


function TGW2APIMisc.GetBuild(aWebHandler: TWebHandler): TGW2Version;
var
  Reply:   string;
  JSObject: TJSONObject;
begin
  Reply    := aWebHandler.FetchEndpoint(APIv2, v2Build, nil);
  JSObject := TJSONObject.ParseJSONValue(Reply) as TJSONObject;
  Result   := TJson.JsonToObject<TGW2Version>(JSObject);
end;


function TGW2APIMisc.GetColorIDs(aWebHandler: TWebHandler): TIntegerArray;
var
  Reply:   string;
  JSArr:   TJSONArray;
  I:       Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Colors, nil);
  JSArr  := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
    Result[I] := StrToInt(JSArr.Items[I].Value);
end;


function TGW2APIMisc.GetColors(aWebHandler: TWebHandler; aParams: TUrlParams): TGW2ColorArray;
var
  Reply:      string;
  JSArr:      TJSONArray;
  JSObject:   TJSONObject;
  I:          Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Colors, aParams);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
  begin
    JSObject     := JSArr.Items[I] as TJSONObject;
    Result[I] := TJson.JsonToObject<TGW2Color>(JSObject);
  end;
end;


function TGW2APIMisc.GetQuagganIDs(aWebHandler: TWebHandler): TStringArray;
var
  Reply:   string;
  JSArr:   TJSONArray;
  I:       Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Quaggans, nil);
  JSArr  := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
    Result[I] := JSArr.Items[I].Value;
end;


function TGW2APIMisc.GetQuaggans(aWebHandler: TWebHandler; aParams: TUrlParams): TGW2QuagganArray;
var
  Reply:        string;
  JSArr:        TJSONArray;
  JSObject:     TJSONObject;
  I:            Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Quaggans, aParams);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
  begin
    JSObject  := JSArr.Items[I] as TJSONObject;
    Result[I] := TJson.JsonToObject<TGW2Quaggan>(JSObject);
  end;
end;


{ Main API class }
//TODO 1 -oThimo -cMain: Add functions/procedures for the API
constructor TGW2API.Create(aTimeoutSeconds: Integer = 15);
begin
  Inherited Create;

  // Initialize the HTTP client
  fStateHolder.HTTPClient                   := TIdHTTP.Create(nil);
  fStateHolder.HTTPClient.Request.UserAgent := 'Mozilla/5.0 (compatible; GW2DelphiAPI/' +
                                               CONST_VERSION_SHORT + ')';

  // Set initial info
  SetTimeout(aTimeoutSeconds);
  fWebHandler            := TWebHandler.Create;
  fWebHandler.HTTPClient := fStateHolder.HTTPClient;
  fUtils                 := TGW2Helper.Create;
  fMisc                  := TGW2APIMisc.Create;
end;


destructor TGW2API.Destroy;
begin
  fStateHolder.HTTPClient.Disconnect;
  FreeAndNil(fWebHandler);
  FreeAndNil(fStateHolder.HTTPClient);
  FreeAndNil(fUtils);
  FreeAndNil(fMisc);
end;


procedure TGW2API.SetTimeout(aSeconds: SmallInt);
begin
  fStateHolder.HTTPTimeout               := aSeconds * CONST_ONE_SECOND;
  fStateHolder.HTTPClient.ConnectTimeout := fStateHolder.HTTPTimeout;
  fStateHolder.HTTPClient.ReadTimeout    := fStateHolder.HTTPTimeout;
end;


function TGW2API.Authenticate(aAuthString: string): string;
var
  AuthToken: TGW2Token;
  StrValue:  string;
begin
  if not TRegEx.IsMatch(aAuthString, '^(?:[A-F\d]{4,20}-?){8,}$') then
  begin
    Result := 'The provided API key does not match the expected format.';
    Exit;
  end;

  fStateHolder.AuthString := aAuthString;
  fStateHolder.AuthToken  := nil;

  AuthToken := fUtils.GW2TokenInfo(fWebHandler, aAuthString);

  for StrValue in AuthToken.Permissions do
    if Result = '' then
      Result := 'Permissions granted to this API key: ' + StrValue
    else
      Result := Result + ', ' + StrValue;

  fStateHolder.AuthToken := AuthToken;
end;

end.
