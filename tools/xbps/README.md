# [XBPS](https://docs.voidlinux.org/xbps/index.html)

> The X Binary Package System (XBPS) is a fast package manager that has been designed and implemented from scratch. XBPS is managed by the Void Linux team and developed at https://github.com/void-linux/xbps. - [voidlinux.org](https://docs.voidlinux.org/xbps/index.html)

**PS:** This document is just a quick-reference, is definitely incomplete, and may even contain errors. For accurate reference, please refer to the [official documentation](https://docs.voidlinux.org/xbps/index.html).

## Searching

List all added repos

    xbps-query -L

List installed packages

    xbps-query -l
    
Search for installed packages

    xbps-query -s <package-name>
    
Search for remote packages

    xbps-query -Rs <package-name>
    
View information about a package

    xbps-query -S <package-name>

## Installing

Install (sync) a package

    xbps-install -S <package-name>

Update a package

    xbps-install -u <package-name>

Update a package (with sync)

    xbps-install -Su <package-name>

Update package indexes

    xbps-install -S

Update all packages

    xbps-install -u

Update all packages (with sync)

    xbps-install -Su

Enable nonfree software

    xbps-install void-repo-nonfree

## Removing

Simply remove the specified package

    xbps-remove <package-name>

Remove recursively

    xbps-remove -R <package-name>
    
Remove orphan packages

    xbps-remove -o
