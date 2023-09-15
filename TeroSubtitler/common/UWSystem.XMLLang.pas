{*
 *  URUWorks XML Language
 *
 *  Based on the great work of CynicRus
 *  Copyright (C) 2022-2023 URUWorks, uruworks@gmail.com.
 *}

unit UWSystem.XMLLang;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, fgl, gvector, ActnList,
  XMLRead, XMLWrite, Dom, TypInfo;

type
  {TOnChangeLang = function(obj: TObject): boolean of object;
  TOnDefaultLang = procedure(obj: TObject);}

  { TAppStrings }

  TAppStringList = specialize TFPGMap<String, String>;

  TAppStrings = class
  private
    FItems: TAppStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddString(const AName, AValue: String);
    function GetString(const AName: String): String;
    procedure LoadFromXML(const AParentNode: TDOMNode);
    procedure SaveToXML(const XMLDoc: TXMLDocument; const AParentNode: TDOMNode);
  end;


  { TLangItem }

  TLangItem = class
  private
    FComponentName: String;
    FComponentCaption: String;
    FComponentHint: String;
    FData: String;
  public
    constructor Create(const Component: TComponent); overload;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromXML(const AParentNode: TDOMNode);
    procedure SaveToXML(const XMLDoc: TXMLDocument; const AParentNode: TDOMNode);
    function toStr: String;
    property ComponentName: String read FComponentName write FComponentName;
    property ComponentCaption: String read FComponentCaption write FComponentCaption;
    property ComponentHint: String read FComponentHint write FComponentHint;
    property Data: String read FData write FData;
  end;

  TLangItemList = specialize TVector<TLangItem>;

  { TSection }

  TSection = class
  private
    FSectionName: String;
    FItems: TLangItemList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddControl(const Component: TComponent);
    procedure ApplyToControl(const Component: TComponent);
    procedure LoadFromXML(const AParentNode: TDOMNode);
    procedure SaveToXML(const XMLDoc: TXMLDocument; const AParentNode: TDOMNode);
    property SectionName: String read FSectionName write FSectionName;
    property Items: TLangItemList read FItems;
  end;

  TSectionList = specialize TVector<TSection>;

  { TFormSection }

  TFormSection = class
  private
    FFormName: String;
    FFormCaption: String;
    FSections: TSectionList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AssignForm(const Form: TForm; const ClassToExclude: array of const);
    procedure LoadFromXML(const AParentNode: TDOMNode);
    procedure SaveToXML(const XMLDoc: TXMLDocument; const AParentNode: TDOMNode);
    procedure ApplyToForm(const Form: TForm);
    property Items: TSectionList read FSections;
    property FormName: String read FFormName write FFormName;
    property FormCaption: String read FFormCaption write FFormCaption;
  end;

  TFormSectionList = specialize TVector<TFormSection>;

  { TLanguageHeader }

  TLanguageHeader = class
  private
    FLangName: String;
    FAuthor: String;
    FAuthorsMail: String;
    FPath: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromXML(const AParentNode: TDOMNode);
    procedure SaveToXML(const AParentNode: TDOMNode);
    property LangName: String read FLangName write FLangName;
    property Path: String read FPath write FPath;
    property Author: String read FAuthor write FAuthor;
    property AuthorsMail: String read FAuthorsMail write FAuthorsMail;
  end;

  { TLanguage }

  TLanguage = class
  private
    FHeader: TLanguageHeader;
    FSections: TFormSectionList;
    FAppStrings: TAppStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromXML(const AFileName: String);
    procedure SaveToXML(const AFileName: String);
    procedure FormToLang(const Form: TForm; const ClassToExclude: array of const);
    procedure LangToForm(const Form: TForm);
    procedure FillHeader(const AFileName: String);
    property Header: TLanguageHeader read FHeader write FHeader;
    property AppStrings: TAppStrings read FAppStrings;
  end;

  TLanguageList = specialize TVector<TLanguage>;

  { TLanguageManager }

  TLanguageManager = class
  private
    FLangs: TLanguageList;
    FCurrentIndex: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetLanguageFolder(const APath: String);
    procedure MakeDefaultLang(const APath: String; const ClassToExclude: array of const);
    function GetLangIndexByName(const AName: String): Integer;
    procedure FillComboBoxWithLangs(const ComboBox: TComboBox);
    procedure FillStringsWithLangs(const St: TStrings);
    procedure ApplyLanguage(const LangIndex: Integer);
    procedure ApplyLanguage(const LangIndex: Integer; const Form: TForm); overload;
    function GetAppString(const LangIndex: Integer; const AName: String): String; overload;
    function GetAppString(const AName: String): String; overload;
    function GetAppStringList(const ASectionName: String; var AList: TAppStringList): Boolean;
  end;

  { Helpers }

  function GetString(const AList: TAppStringList; const AName: String): String;

var
  LanguageManager: TLanguageManager = NIL;

// -----------------------------------------------------------------------------

implementation

uses DateUtils, FileUtil;

// -----------------------------------------------------------------------------

{ Helpers }

// -----------------------------------------------------------------------------

function GetString(const AList: TAppStringList; const AName: String): String;
begin
  if AList <> NIL then
  begin
    if AList.TryGetData(AName, Result) then
      Result := Result.Replace('|', sLineBreak, [rfReplaceAll])
    else
      Result := '';
  end
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function GetNodeByName(const XmlDoc: TXMLDocument; const NodeName: String): TDOMNode;

  function FindNode(ANode: TDOMNode): TDOMNode;
  var
    i: Integer;
  begin
    Result := NIL;
    if Assigned(ANode) then
    begin
      if ANode.NodeName = NodeName then
        Result := ANode
      else
        for i := 0 to ANode.ChildNodes.Count-1 do
        begin
          Result := FindNode(ANode.ChildNodes[i]);
          if Assigned(Result) then Break;
        end;
    end;
  end;

var
  i: Integer;
begin
  Result := NIL;
  if Assigned(XmlDoc) and (XmlDoc.ChildNodes.Count > 0) then
  begin
    for i := 0 to XmlDoc.ChildNodes.Count-1 do
    begin
      Result := FindNode(XmlDoc.ChildNodes[i]);
      if Assigned(Result) then Break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function GetSection(const SectionName: String; const List: TSectionList): TSection;
var
  i: Integer;
begin
  Result := NIL;
  for i := 0 to List.Size-1 do
    if SectionName = List[i].SectionName then
      Result := List[i];
end;

// -----------------------------------------------------------------------------

function GetFormSection(const FormSectionName: String; const List: TFormSectionList): TFormSection;
var
  i: Integer;
begin
  Result := NIL;
  for i := 0 to List.Size-1 do
    if FormSectionName = List[i].FormName then
      Result := List[i];
end;

// -----------------------------------------------------------------------------

function GetComponentProperty(const Component: TComponent; const PropName: String): String;
var
  PropInfo: PPropInfo;
  Str: String;
begin
  Result := '';
  PropInfo := GetPropInfo(Component, PropName);
  if PropInfo <> NIL then
  begin
    Str := GetStrProp(Component, PropInfo);
    Str := StringReplace(Str, sLineBreak, '|', [rfReplaceAll]);
    Result := Str;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetComponentProperty(const Component: TComponent; const PropName, PropValue: String);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(Component, PropName);
  if PropInfo <> NIL then
    SetStrProp(Component, PropName, PropValue);
end;

// -----------------------------------------------------------------------------

{ TAppStrings }

// -----------------------------------------------------------------------------

constructor TAppStrings.Create;
begin
  FItems := TAppStringList.Create;
end;

// -----------------------------------------------------------------------------

destructor TAppStrings.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TAppStrings.AddString(const AName, AValue: String);
begin
  FItems.Add(AName, AValue);
end;

// -----------------------------------------------------------------------------

function TAppStrings.GetString(const AName: String): String;
begin
  if not FItems.TryGetData(AName, Result) then Result := '';
//  Result := FItems[AName];
end;

// -----------------------------------------------------------------------------

procedure TAppStrings.LoadFromXML(const AParentNode: TDOMNode);
var
  i: Integer;
  Node: TDomNode;
  AName, AValue: string;
begin
  AName := '';
  AValue := '';
  FItems.Clear;
  for i := 0 to AParentNode.ChildNodes.Count-1 do
  begin
    Node   := AParentNode.ChildNodes[i];
    AName  := Node.NodeName;
    AValue := Node.TextContent;
    FItems.Add(AName, AValue);
  end;
end;

// -----------------------------------------------------------------------------

procedure TAppStrings.SaveToXML(const XMLDoc: TXMLDocument; const AParentNode: TDOMNode);
var
  Node: TDomNode;
  i: Integer;
begin
  for i := 0 to FItems.Count-1 do
  begin
    Node := XMLDoc.CreateElement(FItems.Keys[i]);
    TDomElement(Node).TextContent := FItems[FItems.Keys[i]];
    Node := aParentNode.AppendChild(Node);
  end;
end;

// -----------------------------------------------------------------------------

{ TLangItem }

// -----------------------------------------------------------------------------

constructor TLangItem.Create(const Component: TComponent);
var
  SS: TStringStream;
begin
  ComponentCaption := GetComponentProperty(Component, 'caption');
  ComponentHint    := GetComponentProperty(Component, 'hint');
  ComponentName    := GetComponentProperty(Component, 'name');
  if Component is TComboBox then
  begin
    SS := TStringStream.Create('');
    try
      TComboBox(Component).Items.SaveToStream(ss);
      Data := ss.DataString;
    finally
      SS.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

constructor TLangItem.Create;
begin
  ComponentName := '';
  ComponentCaption := '';
  ComponentHint := '';
  Data := '';
end;

// -----------------------------------------------------------------------------

destructor TLangItem.Destroy;
begin
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TLangItem.LoadFromXML(const AParentNode: TDOMNode);
begin
  ComponentName    := AParentNode.NodeName;
  ComponentCaption := AParentNode.TextContent;

  if AParentNode.Attributes.GetNamedItem('Hint') <> NIL then
    ComponentHint := AParentNode.Attributes.GetNamedItem('Hint').NodeValue;

  if AParentNode.Attributes.GetNamedItem('Data') <> NIL then
    Data := AParentNode.Attributes.GetNamedItem('Data').NodeValue;
end;

// -----------------------------------------------------------------------------

procedure TLangItem.SaveToXML(const XMLDoc: TXMLDocument; const AParentNode: TDOMNode);
var
  Node: TDomNode;
begin
  if ComponentName.IsEmpty or ComponentCaption.IsEmpty then Exit;

  Node := XMLDoc.CreateElement(ComponentName);
  TDomElement(Node).TextContent := ComponentCaption;

  if ComponentHint <> '' then
    TDomElement(Node).SetAttribute('Hint', ComponentHint);

  if Data <> '' then
    TDomElement(Node).SetAttribute('Data', Data);

  AParentNode.AppendChild(Node);
end;

// -----------------------------------------------------------------------------

function TLangItem.toStr: String;
begin
  Result := {ComponentType + '|' +} ComponentName + '|' + ComponentCaption + '|' + ComponentHint;
end;

// -----------------------------------------------------------------------------

{ TSection }

// -----------------------------------------------------------------------------

constructor TSection.Create;
begin
  FSectionName := '';
  FItems := TLangItemList.Create;
end;

// -----------------------------------------------------------------------------

destructor TSection.Destroy;
begin
  FItems.Clear;
  FItems.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TSection.AddControl(const Component: TComponent);
var
  Item: TLangItem;
begin
  Item := TLangItem.Create(Component);
  FItems.PushBack(Item);
end;

// -----------------------------------------------------------------------------

procedure TSection.ApplyToControl(const Component: TComponent);
var
  Item: TLangItem;
  i: Integer;
  SS: TStringStream;
  s: String;
begin
  // for Item in FItems do
  for i := 0 to Fitems.Size-1 do
  begin
    Item := FItems[i];
    if (Item.ComponentName = GetComponentProperty(Component, 'name')) then
    begin
      if (Component is TAction) then
      begin
        s := Item.ComponentCaption;

        if Item.ComponentHint <> '' then
          s += sLineBreak+sLineBreak + Item.ComponentHint;
      end
      else
        s := Item.ComponentHint;

      SetComponentProperty(Component, 'caption', Item.ComponentCaption);
      SetComponentProperty(Component, 'hint', s);

      if (Component is TComboBox) and (not Item.Data.IsEmpty) then
      begin
        SS := TStringStream.Create(Item.Data);
        TComboBox(Component).Items.LoadFromStream(SS);
        SS.Free;
      end;
      Break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TSection.LoadFromXML(const AParentNode: TDOMNode);
var
  i: integer;
  Item: TLangItem;
  Node: TDomNode;
begin
  SectionName := AParentNode.Attributes.GetNamedItem('Name').NodeValue;
  FItems.Clear;
  for i := 0 to AParentNode.ChildNodes.Count-1 do
  begin
    Node := AParentNode.ChildNodes[i];
    Item := TLangItem.Create;
    Item.LoadFromXML(Node);
    FItems.PushBack(Item);
  end;
end;

// -----------------------------------------------------------------------------

procedure TSection.SaveToXML(const XMLDoc: TXMLDocument; const AParentNode: TDOMNode);
var
  Node: TDomNode;
  Item: TLangItem;
  i: Integer;
begin
  Node := XMLDoc.CreateElement('Section');
  TDOMElement(Node).SetAttribute('Name', SectionName);
  Node := AParentNode.AppendChild(Node);
  for i := 0 to FItems.Size-1 do
  begin
    Item := TLangItem(FItems[i]);
    Item.SaveToXML(XMLDoc, Node);
  end;
end;

// -----------------------------------------------------------------------------

{ TFormSection }

// -----------------------------------------------------------------------------

constructor TFormSection.Create;
begin
  FSections := TSectionList.Create;
end;

// -----------------------------------------------------------------------------

destructor TFormSection.Destroy;
begin
  FSections.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TFormSection.AssignForm(const Form: TForm; const ClassToExclude: array of const);
var
  i, c: Integer;
  Section: TSection;
  Component: TComponent;
  Exclude: Boolean;
begin
  Section := NIL;
  FormName := Form.Name;
  FormCaption := Form.Caption;

  for i := 0 to Form.ComponentCount-1 do
  begin
    Component := Form.Components[i];
    Exclude := False;

    if High(ClassToExclude) < 0 then
      Section := GetSection(Component.ClassName, FSections)
    else
    begin
      for c := 0 to High(ClassToExclude) do
        if AnsiString(ClassToExclude[c].VAnsiString) = Component.ClassName then
        begin
          Exclude := True;
          Break;
        end;

      if not Exclude then
        Section := GetSection(Component.ClassName, FSections);
    end;

    if not Exclude then
    begin
      if Section = NIL then
      begin
        Section := TSection.Create;
        Section.SectionName := Component.ClassName;
        FSections.PushBack(Section);
      end;

      Section.AddControl(Component);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormSection.LoadFromXML(const AParentNode: TDOMNode);
var
  Section: TSection;
  i: Integer;
begin
  if AParentNode.Attributes.GetNamedItem('Name') = NIL then Exit;

  FormName    := AParentNode.Attributes.GetNamedItem('Name').NodeValue;
  FormCaption := AParentNode.Attributes.GetNamedItem('Caption').NodeValue;

  try
    for i := 0 to AParentNode.ChildNodes.Count-1 do
    begin
      Section := GetSection(AParentNode.ChildNodes[i].Attributes.GetNamedItem('Name').NodeValue, FSections);

      if Section = NIL then
      begin
        Section := TSection.Create;
        Section.LoadFromXML(AParentNode.ChildNodes[i]);
        FSections.PushBack(Section);
      end
      else
      begin
        Section.LoadFromXML(AParentNode.ChildNodes[i]);
      end;
    end;
  except
    on E: Exception do
      raise;
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormSection.SaveToXML(const XMLDoc: TXMLDocument; const AParentNode: TDOMNode);
var
  Node: TDomNode;
  i: Integer;
  Section: TSection;
begin
  Node := XMLDoc.CreateElement('Form');
  TDOMElement(Node).SetAttribute('Name', FormName);
  TDOMElement(Node).SetAttribute('Caption', FormCaption);
  Node := AParentNode.AppendChild(Node);
  for i := 0 to FSections.Size-1 do
  begin
    Section := FSections[i];
    Section.SaveToXML(XMLDoc, Node);
  end;
end;

// -----------------------------------------------------------------------------

procedure TFormSection.ApplyToForm(const Form: TForm);
var
  Component: TComponent;
  i: Integer;
  Section: TSection;
begin
  Form.Caption := FormCaption;
  for i := 0 to Form.ComponentCount-1 do
  begin
    Component := Form.Components[i];
    Section := GetSection(Component.ClassName, FSections);

    if (Section = NIL) then Continue;
      {raise Exception.Create('Error! The language section for ' +
      Component.ClassName + ' not found!');}

    Section.ApplyToControl(Component);
  end;
end;

// -----------------------------------------------------------------------------

{ TLanguageHeader }

constructor TLanguageHeader.Create;
begin
  LangName := '';
  Author := '';
  AuthorsMail := '';
end;

// -----------------------------------------------------------------------------

destructor TLanguageHeader.Destroy;
begin
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TLanguageHeader.LoadFromXML(const AParentNode: TDOMNode);
begin
  LangName := AParentNode.Attributes.GetNamedItem('Name').NodeValue;
  Author := AParentNode.Attributes.GetNamedItem('Author').NodeValue;
  AuthorsMail := AParentNode.Attributes.GetNamedItem('Mail').NodeValue;
end;

// -----------------------------------------------------------------------------

procedure TLanguageHeader.SaveToXML(const AParentNode: TDOMNode);
begin
  TDOMElement(aParentNode).SetAttribute('Name', LangName);
  TDOMElement(aParentNode).SetAttribute('Author', Author);
  TDOMElement(aParentNode).SetAttribute('Mail', AuthorsMail);
end;

// -----------------------------------------------------------------------------

{ TLanguage }

// -----------------------------------------------------------------------------

constructor TLanguage.Create;
begin
  FSections := TFormSectionList.Create;
  FHeader := TLanguageHeader.Create;
  FAppStrings := TAppStrings.Create;
  //FAppStrings.AddString('TestKey', 'TestValue');
end;

// -----------------------------------------------------------------------------

destructor TLanguage.Destroy;
begin
  FSections.Clear;
  Fsections.Free;
  FAppStrings.Free;
  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TLanguage.LoadFromXML(const AFileName: String);
var
  xdoc: TXMLDocument;
  Child: TDOMNode;
  Section: TFormSection;
  i: Integer;
  FormList: TDOMNodeList;
begin
  xdoc := TXMLDocument.Create;
  try
    ReadXMLFile(xdoc, AFileName);
    Child := xdoc.DocumentElement;
    FSections.Clear;

    if not Assigned(Child) then Exit;

    if Header.LangName = '' then
      Header.LoadFromXML(Child);

    FormList := Child.GetChildNodes;
    for i := 0 to FormList.Count-1 do
      if FormList.Item[i].NodeName = 'Form' then
      begin
        Section := TFormSection.Create;
        Section.LoadFromXML(FormList.Item[i]);
        FSections.PushBack(Section);
      end;

    AppStrings.LoadFromXML(GetNodeByName(xdoc, 'AppStrings'));
  finally
    FreeAndNil(xdoc);
  end;
end;

// -----------------------------------------------------------------------------

procedure TLanguage.SaveToXML(const AFileName: string);
var
  xdoc: TXMLDocument;
  Node, StringsNode: TDomNode;
  i: Integer;
begin
  xdoc := TXMLDocument.Create;
  try
    Node := xdoc.CreateElement('Language');
    xdoc.AppendChild(Node);
    Node := xdoc.DocumentElement;
    Header.SaveToXML(Node);

    for i := 0 to FSections.Size-1 do
      FSections[i].SaveToXML(xdoc, node);

    StringsNode := xdoc.CreateElement('AppStrings');
    Node.AppendChild(StringsNode);
    FAppStrings.SaveToXML(xDoc, StringsNode);
    WriteXMLFile(xdoc, AFileName);
  finally
    FreeAndNil(xdoc);
  end;
end;

// -----------------------------------------------------------------------------

procedure TLanguage.FormToLang(const Form: TForm; const ClassToExclude: array of const);
var
  Section: TFormSection;
begin
  Section := GetFormSection(Form.Name, FSections);

  if (Section = NIL) then
  begin
    Section := TFormSection.Create;
    Section.AssignForm(Form, ClassToExclude);
    FSections.PushBack(Section);
  end
  else
    Section.AssignForm(Form, ClassToExclude);
end;

// -----------------------------------------------------------------------------

procedure TLanguage.LangToForm(const Form: TForm);
var
  Section: TFormSection;
begin
  Section := GetFormSection(Form.Name, FSections);

  if (Section <> NIL) then
    Section.ApplyToForm(Form);
end;

// -----------------------------------------------------------------------------

procedure TLanguage.FillHeader(const AFileName: String);
var
  xdoc: TXMLDocument;
  Node: TDOMNode;
begin
  xdoc := TXMLDocument.Create;
  try
    ReadXMLFile(xdoc, AFileName);
    Node := xdoc.DocumentElement;
    Header.LoadFromXML(Node);
    Header.Path := AFileName;
  finally
    FreeAndNil(xDoc)
  end;
end;

// -----------------------------------------------------------------------------

{ TLanguageManager }

// -----------------------------------------------------------------------------

procedure TLanguageManager.MakeDefaultLang(const APath: String; const ClassToExclude: array of const);
var
  Lang: TLanguage;
  i: Integer;
begin
  Lang := TLanguage.Create;
  try
    Lang.Header.Author := 'Author';
    Lang.Header.AuthorsMail := 'Author@mail';
    Lang.Header.LangName := 'Default';

    for i := 0 to Application.ComponentCount-1 do
    begin
      if Application.Components[i] is TForm then
        Lang.FormToLang(TForm(Application.Components[i]), ClassToExclude);
    end;
    Lang.SaveToXML(ConcatPaths([APath, Lang.Header.LangName + '.xml']));
  finally
    Lang.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TLanguageManager.GetLangIndexByName(const AName: String): Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to FLangs.Size-1 do
    if ChangeFileExt(ExtractFileName(FLangs.Items[i].Header.Path), '') = AName then
    begin
      Result := i;
      Break;
    end;
end;

// -----------------------------------------------------------------------------

constructor TLanguageManager.Create;
begin
  FLangs := TLanguageList.Create;
  FCurrentIndex := 0;
end;

// -----------------------------------------------------------------------------

destructor TLanguageManager.Destroy;
begin
  if FLangs <> NIL then FLangs.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TLanguageManager.SetLanguageFolder(const APath: String);
var
  Langs: TStringList;
  i: Integer;
  Lang: TLanguage;
begin
  //if not DirectoryExists(APath) then
  //  CreateDir(APath);

  Langs := FindAllFiles(APath, '*.xml', True);

  //if Langs.Count = 0 then
  //  MakeDefaultLang([]);

  for i := 0 to Langs.Count-1 do
  begin
    Lang := TLanguage.Create;
    Lang.FillHeader(Langs[i]);
    FLangs.PushBack(Lang);
  end;
end;

// -----------------------------------------------------------------------------

procedure TLanguageManager.FillComboBoxWithLangs(const ComboBox: TComboBox);
var
  i: Integer;
begin
  Combobox.Items.Clear;
  for i := 0 to Flangs.Size-1 do
    Combobox.Items.Add(Flangs[i].Header.LangName);

  Combobox.ItemIndex := 0;
end;

// -----------------------------------------------------------------------------

procedure TLanguageManager.FillStringsWithLangs(const St: TStrings);
var
  i: Integer;
begin
  St.Clear;
  for i := 0 to FLangs.Size-1 do
    St.Add(FLangs[i].Header.LangName);
end;

// -----------------------------------------------------------------------------

procedure TLanguageManager.ApplyLanguage(const LangIndex: Integer);
var
  Lang: TLanguage;
  i: Integer;
begin
  if FLangs.IsEmpty then Exit;

  Lang := FLangs[LangIndex];
  Lang.LoadFromXML(Lang.Header.Path);
  FCurrentIndex := LangIndex;

  for i := 0 to Application.ComponentCount-1 do
  begin
    if Application.Components[i] is TForm then
      Lang.LangToForm(TForm(Application.Components[i]));
  end;
end;

// -----------------------------------------------------------------------------

procedure TLanguageManager.ApplyLanguage(const LangIndex: Integer; const Form: TForm);
var
  Lang: TLanguage;
begin
  if FLangs.Size > 0 then
  begin
    FCurrentIndex := LangIndex;
    Lang := FLangs[LangIndex];
    Lang.LoadFromXML(Lang.Header.Path);
    Lang.LangToForm(Form);
  end;
end;

// -----------------------------------------------------------------------------

function TLanguageManager.GetAppString(const LangIndex: Integer; const AName: String): String;
var
  Lang: TLanguage;
begin
  if FLangs.Size > 0 then
  begin
    Lang := FLangs[LangIndex];
    Result := Lang.AppStrings.GetString(AName);
  end
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function TLanguageManager.GetAppString(const AName: String): String;
begin
  Result := GetAppString(FCurrentIndex, AName);
end;

// -----------------------------------------------------------------------------

function TLanguageManager.GetAppStringList(const ASectionName: String; var AList: TAppStringList): Boolean;
var
  xdoc: TXMLDocument;
  Child: TDOMNode;
  i: Integer;
  Node: TDomNode;
  AName, AValue: String;
begin
  Result := False;
  if FLangs.IsEmpty then Exit;

  if not Assigned(AList) then
    AList := TAppStringList.Create;

  AList.Clear;
  xdoc := TXMLDocument.Create;
  try
    ReadXMLFile(xdoc, FLangs[FCurrentIndex].Header.Path);
    Child := GetNodeByName(xdoc, ASectionName);
    if not Assigned(Child) then Exit;

    AName  := '';
    AValue := '';
    for i := 0 to Child.ChildNodes.Count-1 do
    begin
      Node   := Child.ChildNodes[i];
      AName  := Node.NodeName;
      AValue := Node.TextContent;
      AList.Add(AName, AValue);
    end;

    Result := AList.Count > 0;
  finally
    FreeAndNil(xdoc);
  end;
end;

// -----------------------------------------------------------------------------

initialization
  LanguageManager := TLanguageManager.Create;

// -----------------------------------------------------------------------------

finalization
  LanguageManager.Free;

// -----------------------------------------------------------------------------

end.
