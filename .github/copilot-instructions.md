# Copilot Instructions for auscyber/dotfiles

This is a personal NixOS/Nix flake-based dotfiles repository that manages system configurations across multiple machines using NixOS, nix-darwin, and Home Manager.

## Repository Structure

- **`systems/`** - NixOS and nix-darwin system configurations organized by architecture
  - `x86_64-linux/` - Linux system configurations
  - `aarch64-darwin/` - macOS system configurations (Apple Silicon)
  - `aarch64-linux-rpi/` - Raspberry Pi configurations
- **`homes/`** - Home Manager user configurations, organized by architecture and user
- **`modules/`** - Reusable Nix modules
  - `home/` - Home Manager modules
  - `nixos/` - NixOS modules
  - `darwin/` - macOS/nix-darwin modules
  - `common/` - Shared modules
- **`packages/`** - Custom package definitions
- **`overlays/`** - Nixpkgs overlays
- **`lib/`** - Custom library functions
- **`flake.nix`** - Main flake configuration
- **`.config/`** - Configuration files (neovim, etc.)

## Build and Test Commands

### Building Configurations
- **Build NixOS system**: `nix build .#nixosConfigurations.<system-name>.config.system.build.toplevel`
- **Build Darwin system**: `nix build .#darwinConfigurations.<system-name>.config.system.build.toplevel`
- **Build Home Manager config**: `nix build .#homeConfigurations.<user@host>.activationPackage`

### Running Checks
- **Run all checks**: `nix run .#ci -- --flake ".#checks.$(nix eval --raw --impure --expr builtins.currentSystem)"`
- **Flake check**: `nix flake check` (may be slow)

### Development
- **Update flake inputs**: `nix flake update`
- **Update documentation**: `nix run .#docs` (updates README.md with system info)
- **Format code**: Use treefmt via `nix fmt`

## CI/CD Workflows

### `.github/workflows/build.yml`
- Runs on: Pull requests, pushes to main/master, manual trigger
- Builds on: Ubuntu and macOS runners
- Steps:
  1. Checks out repository
  2. Installs Nix
  3. Sets up Cachix for binary caching
  4. Builds all checks using `nix run .#ci`
  5. Updates README.md documentation
  6. Commits and pushes README updates

### `.github/workflows/update.yml`
- Runs on: Schedule (2:51 AM on Sundays and Fridays)
- Purpose: Automatically updates flake inputs
- Creates PRs for dependency updates

## Important Conventions

### Nix Flakes
- This repository uses **Nix flakes** exclusively
- All Nix commands should use the flake syntax (e.g., `nix build`, not `nix-build`)
- The `flake.lock` file is committed and should be updated intentionally

### Module Organization
- Modules follow a hierarchical structure
- Use `default.nix` in directories to expose module functionality
- Configuration options should be properly namespaced

### Secrets Management
- Uses **agenix** for secrets management
- Secrets are encrypted with age and stored in the repository
- See `.sops.yaml` for secrets configuration
- Never commit unencrypted secrets

### Code Style
- Use 2-space indentation for Nix files
- Follow standard Nix formatting conventions
- Run `nix fmt` before committing

## Common Tasks

### Adding a New System Configuration
1. Create a directory under `systems/<architecture>/<system-name>/`
2. Add a `default.nix` file with the system configuration
3. System configurations are automatically discovered via lib functions

### Adding a New Home Manager Configuration
1. Create a directory under `homes/<architecture>/<user@host>/`
2. Add a `default.nix` file with the home configuration
3. Home configurations are automatically discovered

### Adding a New Module
1. Place the module in the appropriate directory under `modules/`
2. Ensure proper option definitions using `lib.mkOption`
3. Import the module in the relevant configuration

### Modifying Neovim Configuration
- Neovim config is in `.config/nvim/` using Fennel (Lisp dialect)
- Uses lazy.nvim for plugin management
- Plugin list in `.config/nvim/fnl/plugins.fnl`

## Common Issues

### Build Failures
- If builds fail, check that Nix is properly installed
- Ensure flake inputs are up to date
- Use `--show-trace` for detailed error information
- Check for syntax errors in Nix expressions

### CI Failures
- CI runs on both Linux (Ubuntu) and macOS
- Some packages may not be available on all platforms
- Use `meta.platforms` to restrict packages to specific platforms
- Check Cachix for caching issues

### README Updates
- The README.md is **auto-generated** by `nix run .#docs`
- Do not manually edit the systems/homes tables in README.md
- Manual edits will be overwritten by CI

## Testing Changes

When making changes:
1. Test locally with `nix build` or `nix flake check`
2. Verify no syntax errors
3. Test on the appropriate architecture if possible
4. Let CI validate cross-platform compatibility

## Dependencies and Package Management

- Use `nixpkgs` for standard packages
- Custom packages go in `packages/` directory
- Use overlays for package modifications
- External dependencies are managed via flake inputs
