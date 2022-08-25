# https://github.com/hashicorp/terraform-provider-aws/pull/16373
# https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/ebs_snapshot_import
terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.16"
    }
  }
  required_version = ">= 1.2.0"

  # We use Terraform Cloud to synchronize Terraform state.  Before deploying
  # with Terraform, you'll need to create an account at
  # https://app.terraform.io and make sure you can access
  # https://app.terraform.io/app/binplz/workspaces/binplz.
  #
  # You'll then need to login on the CLI with `terraform login`.
  cloud {
    organization = "binplz"
    workspaces {
      name = "binplz"
    }
  }
}

provider "aws" {
  region                   = "us-west-2"
  shared_credentials_files = ["../secrets/plaintext/aws_credentials"]
}

resource "aws_instance" "binplz_server" {
  ami                         = aws_ami.binplz_ami.id
  instance_type               = "t2.micro"
  vpc_security_group_ids      = [aws_security_group.my_security_group.id]
  user_data_replace_on_change = true

  # We could also use a file provisioner here, but I've found that to be a bit more fragile since it requires SSH access.
  user_data = <<EOF
#!/run/current-system/sw/bin/bash
PATH=/run/current-system/sw/bin
echo "${file("../secrets/plaintext/nixbuild.pem")}" > /root/nixbuild.pem
chmod 0600 /root/nixbuild.pem
EOF
}

output "server_ip_addr" {
  value = aws_instance.binplz_server.public_ip
}

resource "aws_security_group" "my_security_group" {
  ingress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_ami" "binplz_ami" {
  name                = var.binplz_image_name_hash
  virtualization_type = "hvm"
  root_device_name    = "/dev/xvda"

  ebs_block_device {
    device_name = "/dev/xvda"
    snapshot_id = aws_ebs_snapshot_import.binplz_image.id
  }
}

resource "aws_s3_bucket" "binplz_bucket" {}

resource "aws_s3_bucket_acl" "binplz_bucket_acl" {
  bucket = aws_s3_bucket.binplz_bucket.id
  acl    = "private"
}

variable "binplz_ami_path" {
  description = "Path to the binplz server AMI image"
  type        = string
}

variable "binplz_image_name_hash" {
  description = "Name of the image"
  type        = string
}

resource "aws_s3_object" "image_upload" {
  # By default, uploading a new object doesn't invalidate downstream resources as long as the key stays the same.
  # So, putting the hash in the key here makes it so that those resources properly get invalidated.
  # We could also explicitly add a lifcycle.replace_triggered_by clause to every dependee, but I think this is safer.
  bucket = aws_s3_bucket.binplz_bucket.id
  key    = var.binplz_image_name_hash
  source = var.binplz_ami_path
}

resource "aws_ebs_snapshot_import" "binplz_image" {
  disk_container {
    format = "VHD"
    user_bucket {
      s3_bucket = aws_s3_bucket.binplz_bucket.id
      s3_key    = aws_s3_object.image_upload.id
    }
  }
  role_name = aws_iam_role.vmimport_role.id
}

resource "aws_iam_role" "vmimport_role" {
  name = "vmimport"
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect    = "Allow"
        Principal = { Service = "vmie.amazonaws.com" }
        Action    = "sts:AssumeRole"
        Condition = {
          StringEquals = {
            "sts:Externalid" = "vmimport"
          }
        }
      }
    ]
  })
}

resource "aws_iam_role_policy_attachment" "vmimport_attach" {
  role       = aws_iam_role.vmimport_role.id
  policy_arn = aws_iam_policy.vmimport_policy.arn
}

resource "aws_iam_policy" "vmimport_policy" {
  name = "vmimport_trust_policy"
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "s3:GetBucketLocation",
          "s3:GetObject",
          "s3:ListBucket",
          "s3:PutObject",
          "s3:GetBucketAcl"
        ]
        Resource = [
          "arn:aws:s3:::${aws_s3_bucket.binplz_bucket.id}",
          "arn:aws:s3:::${aws_s3_bucket.binplz_bucket.id}/*"
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "ec2:ModifySnapshotAttribute",
          "ec2:CopySnapshot",
          "ec2:RegisterImage",
          "ec2:Describe*"
        ],
        Resource = "*"
      }
    ]
  })
}
