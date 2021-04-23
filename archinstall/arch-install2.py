import archinstall, getpass, os
from archinstall.lib.general import sys_command
# Select a harddrive and a disk password
archinstall.log(f"Minimal only supports:")
archinstall.log(f" * Being installed to a single disk")
USER ="auscyber"

class AurPackageSet:
    def __init__(self,user):
        self.packages = []
        self.user = user

    def __add__(self, other):
        self.packages.append(other)
        return self

    def __str__(self):
        user = self.user
        output = f"""#!/bin/bash
mkdir -p /home/{user}/.cache/
cd /home/{user}/.cache
    """
        for package in self.packages:
            output+= f"""
git clone https://aur.archlinux.org/{package}.git
cd {package}
makepkg -si --noconfirm
cd ..
            """
        return output


packages = [ 'vim',
            'alacritty',
            'openssh',
            'meson',
            'maim',
            'git',
            'wget',
            'nodejs',
            'feh',
            'xclip',
#            'discord',
#            'dunst',
#            'fzf',
#            'lxsession',
#            'lxapperance',
#            'maim',
#            'rofi',
            'zsh']

aur_packages = ['neovim-nightly-bin','yay', 'polybar', "picom-jonaburg-git", "spotify" , "slack", "teams"]

class UserHook:
    def __init__(self, user="auscyber", mount="/mnt", home=None, location=".bashrc", computer="desktop"):
        self.user = user
        self.home = home
        self.mount = mount
        self.location = location
        if home is None:
            self.home = f"/home/{user}"
        self.command = f"""\
#!/bin/bash
set -e
sudo pacman -Syu
cd ~
mv "{self.home}/{location}" "{self.home}/{location}.old"
echo Running Post Install script
sleep 2
git clone https://github.com/auscyberman/dotfiles.git
echo "Copying dotfiles"
mkdir -p ~/.local/bin
wget https://github.com/AusCyberman/dotfile-sync/releases/download/0.01/dots -O ~/.local/bin/dots
chmod +x ~/.local/bin/dots
export PATH=$PATH:~/.local/bin
cd dotfiles
dots --system {computer} sync
"""
        self.yay_done = False
    def __enter__(self):
        return self

    def __exit__(self, a, b, c):
        with open(f"{self.mount}{self.home}/{self.location}","w+") as f:
            f.write(self.command)
        installation.run_command(f"chown {USER} {self.home}/{self.location}")

    def add_aur_packages(self, packages): 
        if not self.yay_done:
            self.command += f"""
            mkdir ~/.cache
            cd ~/.cache
            git clone https://aur.archlinux.org/yay
            cd yay
            makepkg -si --noconfirm
            """
        fixed_pack = ''.join(sum([[a for a in b+' '] for b in packages],[]))
        self.command += f"""
        yay -Sy {fixed_pack}
        """
    def set_shell(self, path="/bin/zsh"):
        self.command += f"chsh -s {path}"

    def copy_to_home(self,path,destination):
        pass
#        sys_command(f"cp \"{path}\" \"{self.mount}{self.home}/{self.destination}\"")

# Select a harddrive and a disk password
harddrive = archinstall.select_disk(archinstall.all_disks())
disk_password = getpass.getpass(prompt='Disk password (leave blank to not encrypt): ')

if disk_password is None:
    print("none")

user_pasword = getpass.getpass(prompt='Enter user password: ')
if a := archinstall.ask_for_a_timezone():
    timezone = a
else:
    timezone = "Australia/Melbourne"
print(timezone)
pc_name = input("Computer name: ")

def install_on(mountpoint):
    # We kick off the installer by telling it where the
    with archinstall.Installer(mountpoint) as installation:
        # Strap in the base system, add a boot loader and configure
        # some other minor details as specified by this profile and user.
        if installation.minimal_installation():
            installation.set_hostname(f"{USER}-{pc_name}")
            installation.add_bootloader()
            # Optionally enable networking:
            installation.copy_ISO_network_config(enable_services=True)
            installation.install_profile('xorg')
            installation.set_timezone(timezone)
            installation.add_additional_packages(packages)
            installation.user_create(USER, user_pasword, sudo=True)
            installation.user_set_pw('root', "toor")
            with open(mountpoint+'/etc/sudoers',"a+") as f:
                f.write("Defaults !requiretty \n Defaults !tty_tickets")
            with UserHook() as h:
                h.set_shell("/bin/zsh")
                h.add_aur_packages(aur_packages)

# Once this is done, we output some useful information to the user
# And the installation is complete.
#    archinstall.log(f"There are two new accounts in your installation after reboot:")
#    archinstall.log(f" * root (password: airoot)")
#    archinstall.log(f" * devel (password: {})")

if harddrive:
    harddrive.keep_partitions = False

    print(f" ! Formatting {harddrive} in ", end='')
    archinstall.do_countdown()

    # First, we configure the basic filesystem layout
    with archinstall.Filesystem(harddrive, archinstall.GPT) as fs:
    	# We use the entire disk instead of setting up partitions on your own
    	if not harddrive.keep_partitions:
    		fs.use_entire_disk(root_filesystem_type='ext4')

    	boot = fs.find_partition('/boot')
    	root = fs.find_partition('/')

    	boot.format('vfat')

    	# We encrypt the root partition if we got a password to do so with,
    	# Otherwise we just skip straight to formatting and installation
    	if disk_password is not None:
    		root.encrypted = True
    		root.encrypt(password=disk_password)

    		with archinstall.luks2(root, 'luksloop', disk_password) as unlocked_root:
    			unlocked_root.format(root.filesystem)
    			unlocked_root.mount('/mnt')
    	else:
    		root.format(root.filesystem)
    		root.mount('/mnt')

    	boot.mount('/mnt/boot')

install_on('/mnt')
