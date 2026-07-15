{
  lib,
  python3,
  fetchFromGitHub,
  fetchPypi,
}:
let
  # pandas-stubs (a check-only input pulled via markitdown[pdf] -> pdfplumber) has
  # broken tests in this nixpkgs pin; disabling checks on the consumers drops it.
  python = python3.override {
    self = python;
    packageOverrides = _final: prev: {
      pdfplumber = prev.pdfplumber.overridePythonAttrs (_: { doCheck = false; });
      markitdown = prev.markitdown.overridePythonAttrs (_: { doCheck = false; });
    };
  };
  pp = python.pkgs;

  # pyzotero isn't in nixpkgs; uv_build backend.
  pyzotero = pp.buildPythonPackage rec {
    pname = "pyzotero";
    version = "1.13.2";
    pyproject = true;

    src = fetchPypi {
      inherit pname version;
      hash = "sha256-Vrj4Jkbox1eTOp7xuu7H6oQZH675pibtIhET70GOevQ=";
    };

    build-system = [ pp.uv-build ];
    dependencies = with pp; [
      feedparser
      bibtexparser
      httpx
      whenever
    ];
    pythonImportsCheck = [ "pyzotero" ];
    doCheck = false;

    meta = {
      description = "Python client for the Zotero API";
      homepage = "https://github.com/urschrei/pyzotero";
      license = lib.licenses.blueOak100;
    };
  };
in
pp.buildPythonApplication rec {
  pname = "zotero-mcp";
  version = "0.6.2";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "54yyyu";
    repo = "zotero-mcp";
    rev = "v${version}";
    hash = "sha256-zTZ40MxZGkmxL5WzALogJ9828rz5fHkTAyDdcLfpva8=";
  };

  build-system = [ pp.hatchling ];

  dependencies = with pp; [
    pyzotero
    mcp
    python-dotenv
    markitdown
    pydantic
    requests
    fastmcp
    unidecode
    bibtexparser
    pymupdf
    ebooklib
  ];

  doCheck = false;
  pythonImportsCheck = [ "zotero_mcp" ];

  meta = {
    description = "Model Context Protocol server for Zotero";
    homepage = "https://github.com/54yyyu/zotero-mcp";
    license = lib.licenses.mit;
    mainProgram = "zotero-mcp";
  };
}
