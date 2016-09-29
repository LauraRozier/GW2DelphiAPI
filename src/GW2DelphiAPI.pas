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

  if fHTTPClient.ResponseCode = 404 then
    raise Exception.Create('Error: API function or value does not exist!');

  if fHTTPClient.ResponseCode = 403 then
    raise Exception.Create('Error: Unauthorized access, please provide a valid API key!');

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


//* Version: 9
//* Class: WebHandler
//* Retrieve the raw reply of a specific URL
//* aUrl: The complete URL that you wish to call
//* Result: Returns the full reply in plain-text
function TWebHandler.FetchRawEndpoint(aUrl: string): string;
var
  Response: TErrorMessage;
begin
  Response := SendRequest(aUrl);

  if Response.HadError then
    raise Exception.Create(Response.Msg);

  Result := Response.Msg;
end;


//* Version: 32
//* Class: WebHandler
//* Retrieve the raw reply of a specific URL
//* aUrl: The complete URL that you wish to call
//* Result: Returns the full reply as an object
function TWebHandler.FetchRawEndpoint<T>(aUrl: string): T;
var
  Response: TErrorMessage;
  JSObject: TJSONObject;
begin
  Response := SendRequest(aUrl);

  if Response.HadError then
    raise Exception.Create(Response.Msg);

  JSObject := TJSONObject.ParseJSONValue(Response.Msg) as TJSONObject;
  Result   := TJson.JsonToObject<T>(JSObject);
end;


//* Version: 9
//* Class: WebHandler
//* Retrieve the raw reply of a specific API version and function with parameters
//* aVersion: The API version enum value
//* aFunction: The API function enum value
//* aParams: An array of parameters, these can be IDs and Language codes
//* Result: Returns the full reply in plain-text
function TWebHandler.FetchEndpoint(aVersion: TAPIVersion; aFunction: TAPIFunction; aParams: TUrlParams): string;
var
  Url:      string;
  Response: TErrorMessage;
begin
  Url := CONST_API_URL_BASE + CONST_API_Versions[aVersion] + '/' + CONST_API_Functions[aFunction];

  if (Length(aParams) > 0) and not (aParams = nil) then
    Url := Url + BuildParamString(aParams);

  Response := SendRequest(Url);

  if Response.HadError then
    raise Exception.Create(Response.Msg);

  Result := Response.Msg;
end;


//* Version: 32
//* Class: WebHandler
//* Retrieve the raw reply of a specific API version and function with parameters
//* aVersion: The API version enum value
//* aFunction: The API function enum value
//* aParams: An array of parameters, these can be IDs and Language codes
//* Result: Returns the full reply as an object
function TWebHandler.FetchEndpoint<T>(aVersion: TAPIVersion; aFunction: TAPIFunction; aParams: TUrlParams): T;
var
  Url:      string;
  Response: TErrorMessage;
  JSObject: TJSONObject;
begin
  Url := CONST_API_URL_BASE + CONST_API_Versions[aVersion] + '/' + CONST_API_Functions[aFunction];

  if (Length(aParams) > 0) and not (aParams = nil) then
    Url := Url + BuildParamString(aParams);

  Response := SendRequest(Url);

  if Response.HadError then
    raise Exception.Create(Response.Msg);

  JSObject := TJSONObject.ParseJSONValue(Response.Msg) as TJSONObject;
  Result   := TJson.JsonToObject<T>(JSObject);
end;


//* Version: 15
//* Class: WebHandler
//* Retrieve the raw reply of a specific API version and function with parameters and authentication
//* aVersion: The API version enum value
//* aFunction: The API function enum value
//* aParams: An array of parameters, these can be IDs and Language codes
//* aAuthString: Your API auth string
//* Result: Returns the full reply in plain-text
function TWebHandler.FetchAuthEndpoint(aVersion: TAPIVersion; aFunction: TAPIFunction; aParams: TUrlParams; aAuthString: string): string;
begin
  if aAuthString = '' then
    raise Exception.Create('This API function requires authentication.');

  SetLength(aParams, Length(aParams) + 1);
  aParams[Length(aParams) - 1].Name  := 'access_token';
  aParams[Length(aParams) - 1].Value := aAuthString;

  Result := FetchEndpoint(aVersion, aFunction, aParams);
end;


//* Version: 32
//* Class: WebHandler
//* Retrieve the raw reply of a specific API version and function with parameters and authentication
//* aVersion: The API version enum value
//* aFunction: The API function enum value
//* aParams: An array of parameters, these can be IDs and Language codes
//* aAuthString: Your API auth string
//* Result: Returns the full reply as an object
function TWebHandler.FetchAuthEndpoint<T>(aVersion: TAPIVersion; aFunction: TAPIFunction; aParams: TUrlParams; aAuthString: string): T;
begin
  if aAuthString = '' then
    raise Exception.Create('This API function requires authentication.');

  SetLength(aParams, Length(aParams) + 1);
  aParams[Length(aParams) - 1].Name  := 'access_token';
  aParams[Length(aParams) - 1].Value := aAuthString;

  Result := FetchEndpoint<T>(aVersion, aFunction, aParams);
end;


{ Utilities }
//* Version: 21
//* Class: Utils
//* aString: Enum value name
//* Result: Returns the enum value from a string
function TGW2Helper.StringToEnum<TEnum>(const aString: string): TEnum;
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


//* Version: 21
//* Class: Utils
//* aEnumValue: Enum value
//* Result: Returns the value of an enum value as a Integer
function TGW2Helper.EnumToInt<TEnum>(const aEnumValue: TEnum): Integer;
begin
  Result := 0;
  Move(aEnumValue, Result, sizeOf(aEnumValue));
end;


//* Version: 21
//* Class: Utils
//* aEnumValue: Enum value
//* Result: Returns the name of an enum value as a string
function TGW2Helper.EnumToString<TEnum>(const aEnumValue: TEnum): string;
begin
  Result := GetEnumName(TypeInfo(TEnum), EnumToInt(aEnumValue));
end;


//* Version: 21
//* Class: Utils
//* aWebHandler: The API webhandler object
//* aAuthStr: The API auth string
//* Result: Returns an API security token
function TGW2Helper.GetTokenInfo(aWebHandler: TWebHandler; aAuthStr: string): TGW2Token;
begin
  Result := aWebHandler.FetchAuthEndpoint<TGW2Token>(APIv2, v2Tokeninfo, nil, aAuthStr);
end;


{ API Misc functions class }
//* Version: 9
//* Class: Misc
//* aWebHandler: The API webhandler object
//* Result: Returns the GW2 build number
function TGW2APIMisc.GetBuild(aWebHandler: TWebHandler): TGW2Version;
begin
  Result := aWebHandler.FetchEndpoint<TGW2Version>(APIv2, v2Build, nil);
end;


//* Version: 12
//* Class: Misc
//* aWebHandler: The API webhandler object
//* Result: Returns an array of color IDs
function TGW2APIMisc.GetColorIDs(aWebHandler: TWebHandler): TIntegerArray;
var
  Reply:   string;
  JSArr:   TJSONArray;
  I:       Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Colors, nil);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
    Result[I] := StrToInt(JSArr.Items[I].Value);
end;


//* Version: 12
//* Class: Misc
//* aWebHandler: The API webhandler object
//* aParams: The parameters (ids and lang)
//* Result: Returns an array of color objects
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
    JSObject  := JSArr.Items[I] as TJSONObject;
    Result[I] := TJson.JsonToObject<TGW2Color>(JSObject);
  end;
end;


//* Version: 10
//* Class: Misc
//* aWebHandler: The API webhandler object
//* Result: Returns an array of Quaggan IDs
function TGW2APIMisc.GetQuagganIDs(aWebHandler: TWebHandler): TStringArray;
var
  Reply:   string;
  JSArr:   TJSONArray;
  I:       Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Quaggans, nil);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
    Result[I] := JSArr.Items[I].Value;
end;


//* Version: 23
//* Class: Misc
//* aWebHandler: The API webhandler object
//* aParams: The parameters (ids)
//* Result: Returns an array of Quaggan objects
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


//* Version: 25
//* Class: Misc
//* aWebHandler: The API webhandler object
//* Result: Returns an array of world IDs
function TGW2APIMisc.GetWorldIDs(aWebHandler: TWebHandler): TIntegerArray;
var
  Reply:   string;
  JSArr:   TJSONArray;
  I:       Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Worlds, nil);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
    Result[I] := StrToInt(JSArr.Items[I].Value);
end;


//* Version: 25
//* Class: Misc
//* aWebHandler: The API webhandler object
//* aParams: The parameters (ids and lang)
//* Result: Returns an array of world objects
function TGW2APIMisc.GetWorlds(aWebHandler: TWebHandler; aParams: TUrlParams): TGW2WorldArray;
var
  Reply:        string;
  JSArr:        TJSONArray;
  JSObject:     TJSONObject;
  I:            Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Worlds, aParams);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
  begin
    JSObject  := JSArr.Items[I] as TJSONObject;
    Result[I] := TJson.JsonToObject<TGW2World>(JSObject);
  end;
end;


//* Version: 28
//* Class: Misc
//* aWebHandler: The API webhandler object
//* Result: Returns an array of currency IDs
function TGW2APIMisc.GetCurrencyIDs(aWebHandler: TWebHandler): TIntegerArray;
var
  Reply:   string;
  JSArr:   TJSONArray;
  I:       Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Currencies, nil);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
    Result[I] := StrToInt(JSArr.Items[I].Value)
end;


//* Version: 28
//* Class: Misc
//* aWebHandler: The API webhandler object
//* aParams: The parameters (ids and lang)
//* Result: Returns an array of currency objects
function TGW2APIMisc.GetCurrencies(aWebHandler: TWebHandler; aParams: TUrlParams): TGW2CurrencyArray;
var
  Reply:        string;
  JSArr:        TJSONArray;
  JSObject:     TJSONObject;
  I:            Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Currencies, aParams);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
  begin
    JSObject  := JSArr.Items[I] as TJSONObject;
    Result[I] := TJson.JsonToObject<TGW2Currency>(JSObject);
  end;
end;


//* Version: 29
//* Class: Misc
//* aWebHandler: The API webhandler object
//* Result: Returns an array of file IDs
function TGW2APIMisc.GetFileIDs(aWebHandler: TWebHandler): TStringArray;
var
  Reply:   string;
  JSArr:   TJSONArray;
  I:       Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Files, nil);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
    Result[I] := JSArr.Items[I].Value;
end;


//* Version: 29
//* Class: Misc
//* aWebHandler: The API webhandler object
//* aParams: The parameters (ids)
//* Result: Returns an array of file objects
function TGW2APIMisc.GetFiles(aWebHandler: TWebHandler; aParams: TUrlParams): TGW2FileArray;
var
  Reply:        string;
  JSArr:        TJSONArray;
  JSObject:     TJSONObject;
  I:            Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Files, aParams);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
  begin
    JSObject  := JSArr.Items[I] as TJSONObject;
    Result[I] := TJson.JsonToObject<TGW2File>(JSObject);
  end;
end;


//* Version: 30
//* Class: Misc
//* aWebHandler: The API webhandler object
//* Result: Returns an array of Mini IDs
function TGW2APIMisc.GetMiniIDs(aWebHandler: TWebHandler): TIntegerArray;
var
  Reply:   string;
  JSArr:   TJSONArray;
  I:       Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Minis, nil);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
    Result[I] := StrToInt(JSArr.Items[I].Value)
end;


//* Version: 30
//* Class: Misc
//* aWebHandler: The API webhandler object
//* aParams: The parameters (ids and lang)
//* Result: Returns an array of Mini objects
function TGW2APIMisc.GetMinis(aWebHandler: TWebHandler; aParams: TUrlParams): TGW2MiniArray;
var
  Reply:        string;
  JSArr:        TJSONArray;
  JSObject:     TJSONObject;
  I:            Integer;
begin
  Reply := aWebHandler.FetchEndpoint(APIv2, v2Minis, aParams);
  JSArr := TJSONObject.ParseJSONValue(Reply) as TJSONArray;
  SetLength(Result, JSArr.Count);

  for I := 0 to JSArr.Count - 1 do
  begin
    JSObject  := JSArr.Items[I] as TJSONObject;
    Result[I] := TJson.JsonToObject<TGW2Mini>(JSObject);
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


//* Version: 9
//* Class: GW2API
//* Sets the read/write timeout for the websocket
//* aSeconds: Number of seconds
procedure TGW2API.SetTimeout(aSeconds: SmallInt);
begin
  fStateHolder.HTTPTimeout               := aSeconds * CONST_ONE_SECOND;
  fStateHolder.HTTPClient.ConnectTimeout := fStateHolder.HTTPTimeout;
  fStateHolder.HTTPClient.ReadTimeout    := fStateHolder.HTTPTimeout;
end;


//* Version: 9
//* Class: GW2API
//* Sets the security token for this API session
//* aAuthString: The API auth string
//* Result: Returns an error or string of permissions
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

  AuthToken := fUtils.GetTokenInfo(fWebHandler, aAuthString);

  for StrValue in AuthToken.Permissions do
    if Result = '' then
      Result := 'Permissions granted to this API key: ' + StrValue
    else
      Result := Result + ', ' + StrValue;

  fStateHolder.AuthToken := AuthToken;
end;

end.
