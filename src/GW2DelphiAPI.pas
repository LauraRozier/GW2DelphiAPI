unit GW2DelphiAPI;

interface
uses
  // System units
  SysUtils, RegularExpressions,
  // Indy units
  IdHTTP, IdException, IdExceptionCore, IdStack,
  // GW2 Delphi API Units
  GW2DA_Defaults, GW2DA_WebHandlers, GW2DA_Misc;

type
  TStateHoler = record
    HTTPTimeout: Integer;
    HTTPClient:  TIdHTTP;
    AuthString:  string;
    AuthFlags:   Cardinal;
  end;

  TGW2API = class(TObject)
    private
      fStateHolder: TStateHoler;
      fWebHandler:  TWebHandler;
    Public
      Misc: TGW2APIMisc;
      constructor Create(aTimeoutSeconds: Integer = 15);
      destructor Destroy;
      procedure SetTimeout(aSeconds: SmallInt);
      function Authenticate(aAuthString: string): string;
      Property State:      TStateHoler Read fStateHolder;
      Property WebHandler: TWebHandler Read fWebHandler;
  end;

implementation

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
  Misc                   := TGW2APIMisc.Create;
end;


destructor TGW2API.Destroy;
begin
  fStateHolder.HTTPClient.Disconnect;
  FreeAndNil(fWebHandler);
  FreeAndNil(fStateHolder.HTTPClient);
end;


procedure TGW2API.SetTimeout(aSeconds: SmallInt);
begin
  fStateHolder.HTTPTimeout               := aSeconds * CONST_ONE_SECOND;
  fStateHolder.HTTPClient.ConnectTimeout := fStateHolder.HTTPTimeout;
  fStateHolder.HTTPClient.ReadTimeout    := fStateHolder.HTTPTimeout;
end;


function TGW2API.Authenticate(aAuthString: string): string;
begin
  if not TRegEx.IsMatch(aAuthString, '^(?:[A-F\d]{4,20}-?){8,}$')  then
  begin
    Result := 'The provided API key does not match the expected format.';
    Exit;
  end;

  fStateHolder.AuthString := aAuthString;
  fStateHolder.AuthFlags  := 0;

  //TODO 2 -oThimo -cMain: Add code to authenticate this session
end;

end.
