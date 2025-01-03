import archinstall, getpass, os
import pty
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
            'pulseaudio',
#            'discord',
#            'dunst',
#            'fzf',
#            'lxsession',
#            'lxapperance',
#            'maim',
#            'rofi',
            'zsh']

aur_packages = ['neovim-nightly-bin','yay', 'polybar', "picom-jonaburg-git", "spotify" , "slack", "teams"]

##class UserHook:
#    def __init__(self, installation,user="auscyber", mount="/mnt", home=None, location=".bashrc", computer="desktop"):
#        self.installation = installation
#        self.user = user
#        self.home = home
#        self.mount = mount
#        self.location = location
#        if home is None:
#            self.home = f"/home/{user}"
#        self.command = f"""\
##!/bin/bash
#set -e
#echo Running Post Install script
#for i in `env | sed 's/=.*//'` ; do
#    unset $i
#done
#source /etc/profile
#sudo pacman -Syu
#cd {self.home}
#mv "{self.home}/{location}" "{self.home}/{location}.old"
#sleep 2
#git clone https://github.com/auscyberman/dotfiles.git
#echo "Copying dotfiles"
#mkdir -p ~/.local/bin
#wget https://github.com/AusCyberman/dotfile-sync/releases/download/0.01/dots -O ~/.local/bin/dots
#chmod +x ~/.local/bin/dots
#export PATH=$PATH:~/.local/bin
#cd dotfiles
#dots --system {computer} sync
#"""
#        self.yay_done = False
#    def __enter__(self):
#        return self
#
#    def __exit__(self, a, b, c):
#        actual_loc =  f"{self.home}/{self.location}"
#        with open(self.mount+actual_loc,"w+") as f: 
#            f.write(self.command)
#        self.installation.run_command("chmod +x "+actual_loc)
#        pty.spawn(["/usr/bin/arch-chroot","-u",self.user,self.mount,"bash",actual_loc])  
#    def add_aur_packages(self, packages): 
#        if not self.yay_done:
#            self.command += f"""
#            mkdir ~/.cache
#            cd ~/.cache
#            git clone https://aur.archlinux.org/yay
#            cd yay
#            makepkg -si --noconfirm
#            """
#        fixed_pack = ''.join(sum([[a for a in b+' '] for b in packages],[]))
#        self.command += f"""
#        yay -Sy {fixed_pack}
#        """
#    def set_shell(self, path="/bin/zsh"):
#        self.command += f"chsh -s {path}"
#
#    def copy_to_home(self,path,destination):
#        pass
#        sys_command(f"cp \"{path}\" \"{self.mount}{self.home}/{self.destination}\"")

def get_partition(disk, mountpoint,message="Select partition"):
    for (num, partition) in enumerate(disk.partition):
        print(f"({num}): {partition}")
    if (index_str := input(message+': ')):
        index = int(index_str)
        if index < len(disk.partition) and index > -1:
            return disk.partition[index]
        else:
            raise archinstall.DiskError("Partition is out of list")


# Select a harddrive and a disk password
harddrive = archinstall.select_disk(archinstall.all_disks())
extra_partitions = {}
if len(list(filter(lambda x : x.filesystem_supported, harddrive.partition))) > 0:
    if (option := archinstall.ask_for_disk_layout()) == 'abort':
        archinstall.log(f"Safely aborting the installation. No changes to the disk or system has been made.")
        exit(1)
    elif option == 'keep-existing':
        boot = get_partition(harddrive, "/boot",message="Choose boot partition")
        root = get_partition(harddrive,"/",message="Choose root partition")
    elif option == 'format-all':
        boot = None
        root = None
        print("formatting entire disk")
else:
    boot = None
    root = None

while (input("Continue? ")) == "y":
    mountpoint = input("Mountpoint: ")
    disk = archinstall.select_disk(archinstall.all_disks())
    part = get_partition(disk,mountpoint)
    extra_partitions[mountpoint] = {'disk' : disk, 'part' : part}



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
    with archinstall.Installer(mountpoint) as i:
        # Strap in the base system, add a boot loader and configure
        # some other minor details as specified by this profile and user.
        if i.minimal_installation():
            i.set_hostname(f"{USER}-{pc_name}")
            i.add_bootloader()
            i.install_profile('xorg')
            i.set_timezone(timezone)
            i.add_additional_packages(packages)
            i.user_create(USER, user_pasword, sudo=True)
            i.user_set_pw('root', "toor")
            i.add_additional_packages("networkmanager")
            i.enable_service('NetworkManager.service')
            with open(mountpoint+'/etc/sudoers',"a+") as f:
                f.write("Defaults !requiretty \n Defaults !tty_tickets")
            i.arch_chroot(f"su {user} -c 'cd $(mktemp -d) && git clone https://aur.archlinux.org/yay-bin.git . && makepkg -sim --noconfirm'")
            i.arch_chroot(f'su {user} -c "yay -Syu --needed --noconfirm {" ".join(dependencies_aur)}"')
            i.arch_chroot(f'su {user} -c "curl -L https://nixos.org/nix/install | sh"')
# Once this is done, we output some useful information to the user
# And the installation is complete.
#    archinstall.log(f"There are two new accounts in your installation after reboot:")
#    archinstall.log(f" * root (password: airoot)")
#    archinstall.log(f" * devel (password: {})")

if harddrive:
    if root is None or boot is None:
        harddrive.keep_partitions = False
        print("using whole drive")

    print(f" ! Formatting {harddrive} in ", end='')
    archinstall.do_countdown()

    # First, we configure the basic filesystem layout
    with archinstall.Filesystem(harddrive, archinstall.GPT) as fs:
        # We use the entire disk instead of setting up partitions on your own
        if not harddrive.keep_partitions:
            fs.use_entire_disk(root_filesystem_type='ext4')
            boot = fs.find_partition('/boot')
            root = fs.find_partition('/')

        root.allow_formatting = True
        if not input("keep_boot?: "):
            boot.allow_formatting = True
            boot.format('vfat')

        for mp in extra_partitions:
            mp['part'].mount('/mnt'+mp)
        # We encrypt the root partition if we got a password to do so with,
        # Otherwise we just skip straight to formatting and installation
        if len(disk_password) > 0:
            print("encrypting")
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
