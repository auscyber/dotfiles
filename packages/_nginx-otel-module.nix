{
  abseil-cpp,
  c-ares,
  cmake,
  fetchFromGitHub,
  grpc,
  lib,
  mkNginxPlugin,
  nlohmann_json,
  openssl,
  opentelemetry-cpp,
  pkg-config,
  protobuf,
  re2,
  zlib,
}:
# https://github.com/nginxinc/nginx-otel -- not yet in this pin of nixpkgs,
# but this IS the real upstream package.nix (unreleased at our pin), from
# https://github.com/Kranzes/nixpkgs/commit/b6c987c801d1bd4642f99a89ecf3d803732b3918
# -- same author as https://github.com/NixOS/nixpkgs/pull/537190, which added
# dynamic-module support specifically to make this module installable at all
# (it has no static build path: `config`/`config.make` at its root shell out
# to cmake rather than following nginx's own `--add-module=` conventions).
#
# `-DCMAKE_LIBRARY_OUTPUT_DIRECTORY=$out/modules` is the actual fix, not a
# postInstall copy: nginx-otel's `config`/`config.make` are a minimal shim
# that adds a `modules:` build target but never sets
# `ngx_module_type=`/`ngx_module_name=` the way a "proper" nginx module's
# `config` does, so nginx's own install step never learns the module exists
# and would otherwise leave the .so sitting in objs/ uncopied. Pointing
# cmake's own output directory at $out/modules sidesteps that gap instead of
# working around it after the fact.
mkNginxPlugin (finalAttrs: {
  pname = "otel";
  version = "0.1.2";

  src = fetchFromGitHub {
    owner = "nginxinc";
    repo = "nginx-otel";
    tag = "v${finalAttrs.version}";
    hash = "sha256-pGe+1nPH8zUQhcyVxH5/nxwNFMsoOYCUecuDVs1rS4o=";
  };

  # nginx-otel can only be loaded dynamically.
  dynamic = true;

  # nginx's `mapModules` in this pin only folds a module's `buildInputs` (and
  # `nginxPatches`) into its own -- no `nativeBuildInputs` hook -- so cmake
  # and the protoc/grpc_cpp_plugin providers go in buildInputs too, same
  # shape nixpkgs' own njs module uses for its `which` dependency. Listed in
  # both anyway (matching upstream) so nothing has to change if this repo's
  # nixpkgs pin later gains that hook.
  nativeBuildInputs = [
    cmake
    pkg-config
    protobuf
    grpc
  ];

  buildInputs = [
    # cmake and pkg-config are build TOOLS, not libraries -- they're only
    # useful here because this pin's `mapModules` has no
    # `nativeBuildInputs` hook to fold the ones above into nginx's own
    # build, unlike upstream's newer pin. Duplicated rather than moved so
    # nothing has to change if this repo's nixpkgs later gains that hook.
    cmake
    pkg-config
    grpc
    protobuf
    opentelemetry-cpp
    c-ares
    re2
    abseil-cpp
    nlohmann_json
    openssl
    zlib
  ];

  # Read by the module's `config` script (NGX_OTEL_GRPC=package /
  # NGX_OTEL_SDK=package point it at nixpkgs' own gRPC/otel-cpp instead of
  # network-fetching its own pinned copies -- no sandbox access for that
  # here) and CMAKE_LIBRARY_OUTPUT_DIRECTORY, which is what actually gets the
  # built .so into $out/modules (see the file comment above).
  preConfigure = ''
    export NGX_OTEL_CMAKE_OPTS="-DNGX_OTEL_GRPC=package -DNGX_OTEL_SDK=package -DNGX_OTEL_PROTO_DIR=${finalAttrs.passthru.otel-proto} -DCMAKE_LIBRARY_OUTPUT_DIRECTORY=$out/modules"
  '';

  passthru.otel-proto = fetchFromGitHub {
    owner = "open-telemetry";
    repo = "opentelemetry-proto";
    rev = "v1.10.0";
    hash = "sha256-RJrS0C4GZfUdETff+ZlbJr67Z+JObrLsDvyGqobf4UI=";
  };

  meta = {
    description = "OpenTelemetry support for nginx";
    homepage = "https://github.com/nginxinc/nginx-otel";
    license = lib.licenses.asl20;
    platforms = lib.platforms.linux;
  };
})
