# Development version of Replication Package

This is the dev version of the package for

> Land Use, Structural Change and Urban Expansion

which should be accessed in it's full form at zenodo following [this link](https://zenodo.org/records/15814459)

**This repository does not contain any data, so you cannot use it to replicate our results. Use the zenodo deposit.**

## building the container

While you cannot use this to replicator our results, you can still build the docker container which is going to prepare and install all add-on packages for R and julia. And while running the container requires a valid STATA license, the build setup will work without this. To build the raw image from the Dockerfile you have to execute this command:

```bash
cd .devcontainer  # navigate into devcontainer folder
docker build -f Dockerfile -t land-use ..  
```

It will be based on the docker-stata image of the AEA Data Editor, it will add R from the rocker project, and install a bunch of linux support libraries needed as dependencies for the later R packages.

## Mount Package Inside Container and Install Packages

Next step is to use the `devcontainer` extension in VSCode to take care of mounting the content of this package (on your local machine) inside our newly built container (i.e., inside another computer if you will). 

1. You should open the package in vscode (`File > Open Folder` and navigate to the root of this package).
2. Open the command palette (Cmd+shift+P on my mac, click the cogwheel bottom left of the vscode window if not) and type : `devcontainers : rebuild and reopen in container`. This will only work if you installed the devcontainer extension as required.
3. This will launch the process of downloading and installing all julia and R packages needed for our package. This process takes another 30 minutes.
4. You will see that the installation finished successfully upon receiving this output in the VSCode terminal:
   
    ```bash
    🎉 Container setup completed successfully!
    ```
5. Your container is now up and running. The terminal will show something like this:
   
    ```bash
    statauser@292aba2a9dfe:/workspace$ 
    ```




