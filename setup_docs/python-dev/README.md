# Python Workflow
This folder contains scripts meant to set up the python environments and tools I use for scientific work. The way I manage this is currently changing, and documentation will need to be updated to keep track of what I've done.

## Conda
In the past I have managed packages and environments with Conda. This is probably going to stop being the case very soon. Most recently I was trying to get Jupyter widgets updated and found that solving my environment.yml took micromamba 10 seconds, but Conda thought about it for about ten _minutes_ before I gave up.

## pipx
	[todo]

## Micromamba
	[todo]

The function 'j_mamba' defined in `~/.config/.environment_site` will launch a jupyter server using the `jupyter` kernel installed by micromamba. This is the one to update if changing anything in the jupyter config. Because the location of kernels that Jupyter looks for is found in `~/.local/share/juputer/`, kernels created through Conda will also be found by this [[micromamba]] installed Jupyter server. That gives me a relaxed timeline on which to port the full workflow to Micromamba, but it should still be done eventually (i.e., before the next computer move...)

## Jupyter


# Files
This is a list of the directories and files in the present directory, and what they're for.


| `jupyter.yml` | Old Jupyter server environment. Use `jupy2.yml` instead.                                                                                                  |
| `jupy2.yml`   | New Jupyter server environment. This will use more modern builds, and have a minimal core requirements file so that dependencies are built automatically. |

# TODO
- [ ] Move conda environments to a directory in this folder.
- [ ] Make script that installs all conda environments, with upgrade all and remove all options from the environment directory.
