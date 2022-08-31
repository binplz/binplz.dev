# How to Deploy to AWS

This section explains how to deploy binplz.dev to AWS.
We mainly use [Terraform](https://www.terraform.io/) for deployments.

Before running any commands in this section, you'll first need to get into a
Nix shell with `terraform` and other tools available:

```console
$ nix develop .#deploy
```

The commands in this section all assume you're in this deployment shell, and in
this current `deploy/` directory.

## First Time

There are a few steps you'll only need to perform the first time you deploy.

1.  Get an invite to our Terraform Cloud workspace:
    <https://app.terraform.io/app/binplz/workspaces/binplz>.  One of our existing members
    can go to the [User
    Settings](https://app.terraform.io/app/binplz/settings/users) page and send
    an invite to your email address.  Once you create an account and login, make
    sure you can see the `binplz` workspace.

    We use Terraform Cloud to manage remote Terraform state.  This is important
    so that all of our members can deploy without stepping on each other's
    toes.

1.  Login to the above account:

    ```console
    $ terraform login
    ```

1. Generate the plaintext secrets:

    ```console
    $ ../secrets/decrypt.sh -i ~/.ssh/id_rsa
    ```

    This should populate the `./secrets/plaintext` directory.

    The `-i` argument should be the SSH private key for your public key
    specified in [`./secrets/trustees`](./secrets/trustees).

1.  Initialize this current `./deploy/` directory with Terraform:

    ```console
    $ terraform init
    ```

Now you should be ready to do your first deploy.

## Deploy

These steps will have to be run everytime you want to do a deploy to AWS.

1.  Ask Terraform what will be changed if you were to do a deploy:

    ```console
    $ terraform plan
    ```

    Terraform will output all the changes it plans to make to the
    infrastructure.  Make sure you read these changes and confirm they make
    sense.

1.  After you've confirmed the above changes look good, perform the actual deploy:

    ```console
    $ terraform apply
    ```

This `terraform apply` command should be idempotent, so you can safely run it
multiple times.

## Confirming a Sucessful Deploy

After deploying, there are a few ways you can confirm that a deployment was
successful.

-   Checkout the AWS Management Console.  If you login to the AWS Management
    Console, you should be able to look at EC2 and see our instance running.

-   Connect to the machine with SSH:

    ```console
    $ ssh root@$(terraform output -raw server_ip_addr)
    ```

    You should be able to run commands like `ps` or `systemctl` to confirm the
    binplz server is running.

-   Try accessing the API:

    ```console
    $ curl -v http://$(terraform output -raw server_ip_addr)/vim > vim
    $ chmod +x ./vim
    $ ./vim
    ```

## Bringing Down a Deployment

**WARNING: You probably don't want to do this!**

If you want to stop the binplz service from running, you can delete all AWS
resources with the following command:

```console
$ terraform destroy
```
