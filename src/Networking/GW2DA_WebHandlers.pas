unit GW2DA_WebHandlers;

interface
uses
  // System units
  SysUtils,
  // Indy units
  IdHTTP, IdException, IdExceptionCore, IdStack, IdSSLOpenSSL,
  // GW2 Delphi API Units
  GW2DA_Types, GW2DA_Defaults;

type
  TWebHandler = class(TObject)
    private
      fHTTPClient: TIdHTTP;
      function SendRequest(aUrl: string): TErrorMessage;
      function BuildParamString(aParams: TUrlParams): string;
    public
      constructor Create;
      function FetchRawEndpoint(aUrl: string): string;
      function FetchEndpoint(aVersion:  TAPIVersion;
                             aFunction: TAPIFunction;
                             aParams:   TUrlParams): string;
      function FetchAuthEndpoint(aUrl: string): string;
      Property HTTPClient: TIdHTTP Read fHTTPClient Write fHTTPClient;
  end;

implementation

constructor TWebHandler.Create;
begin
  Inherited Create;
end;


function TWebHandler.SendRequest(aUrl: string): TErrorMessage;
var
  IResponse: string;
  IError:    TErrorMessage;
begin
   IError.HadError := False;
   IError.Msg := '';

  try
    IResponse := fHTTPClient.Get(aUrl);
  except
    // Indy protocol exception
    on E:EIdHTTPProtocolException do
    begin
      IError.Msg := 'Error: Indy raised a protocol error!'       + sLineBreak +
                    'HTTP status code: ' + IntToStr(E.ErrorCode) + sLineBreak +
                    'Error message'      + E.Message             + sLineBreak;
      IError.HadError := True;
    end;
    // Indy SSL Library exception
    on E:EIdOSSLCouldNotLoadSSLLibrary do
    begin
      IError.Msg := 'Error: Indy could not load SSL library!' + sLineBreak +
                    'Exception class: ' + E.ClassName                                + sLineBreak +
                    'Error message: '   + E.Message                                  + sLineBreak;
      IError.HadError := True;
    end;
    // Indy server closed connection exception
    on E:EIdConnClosedGracefully do
    begin
      IError.Msg := 'Error: Indy reports, that connection was closed by the server!' + sLineBreak +
                    'Exception class: ' + E.ClassName                                + sLineBreak +
                    'Error message: '   + E.Message                                  + sLineBreak;
      IError.HadError := True;
    end;
    // Indy low-level socket exception
    on E:EIdSocketError do
    begin
      IError.Msg := 'Error: Indy raised a socket error!'    + sLineBreak +
                    'Error code: '  + IntToStr(E.LastError) + sLineBreak +
                    'Error message' + E.Message             + sLineBreak;
      IError.HadError := True;
    end;
    // Indy read-timeout exception
    on E:EIdReadTimeout do
    begin
      IError.Msg := 'Error: Indy raised a read-timeout error!' + sLineBreak +
                    'Exception class: ' + E.ClassName          + sLineBreak +
                    'Error message: '   + E.Message            + sLineBreak;
      IError.HadError := True;
    end;
    // All other Indy exceptions
    on E:EIdException do
    begin
      IError.Msg := 'Error: Something went wrong with Indy!' + sLineBreak +
                    'Exception class: ' + E.ClassName        + sLineBreak +
                    'Error message: '   + E.Message          + sLineBreak;
      IError.HadError := True;
    end;
    // All other Delphi exceptions
    on E:Exception do
    begin
      IError.Msg := 'Error: Something non-Indy related raised an exception!' + sLineBreak +
                    'Exception class: ' + E.ClassName                        + sLineBreak +
                    'Error message: '   + E.Message                          + sLineBreak;
      IError.HadError := True;
    end;
  end;

  if not IError.HadError then
    IError.Msg := IResponse;

  Result := IError;
end;


function TWebHandler.BuildParamString(aParams: TUrlParams): string;
var
  IParam:     TUrlParam;
  IResultStr: string;
begin
  IResultStr := '';

  for IParam in aParams do
  begin
    if IResultStr = '' then
      IResultStr := '?' + IParam.Name + '=' + IParam.Value
    else
      IResultStr := IResultStr + '&' + IParam.Name + '=' + IParam.Value;
  end;

  Result := IResultStr;
end;


function TWebHandler.FetchRawEndpoint(aUrl: string): string;
begin
   Result := SendRequest(aUrl).Msg;
end;


function TWebHandler.FetchEndpoint(aVersion:  TAPIVersion;
                                   aFunction: TAPIFunction;
                                   aParams:   TUrlParams): string;
var
  IUrl, IResponseMsg: string;
  IResponse         : TErrorMessage;
begin
  IUrl := CONST_API_URL_BASE + CONST_API_Versions[aVersion] + '/' + CONST_API_Functions[aFunction];

  if (Length(aParams) > 0) and not (aParams = nil) then
    IUrl := IUrl + BuildParamString(aParams);

  IResponse := SendRequest(IUrl);

  if IResponse.HadError then
    raise Exception.Create('Something went wrong with this request!');

  Result := IResponse.Msg;
end;


function TWebHandler.FetchAuthEndpoint(aUrl: string): string;
begin
  //TODO 2 -oThimo -cWeb: Add code to fetch data from the endpoint with authentication
  Result := SendRequest(aUrl).Msg;
end;

end.
