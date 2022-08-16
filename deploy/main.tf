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
  name = "my_security_group"
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
  # By default, the AMI resource doesn't get invalidated when the image/snapshot changes.
  # The hash in the name here makes it so that a new upstream image always triggers regenerating the AMI.
  # We could probably also explicitly add a `depends_on` clause here instead.
  name                = var.binplz_image_name_hash
  virtualization_type = "hvm"
  root_device_name    = "/dev/xvda"

  ebs_block_device {
    device_name = "/dev/xvda"
    snapshot_id = aws_ebs_snapshot_import.binplz_image.id
  }
}

resource "aws_s3_bucket" "binplz_bucket" {
  bucket_prefix = "my-tf-test-bucket"
}

resource "aws_s3_bucket_acl" "binplz_bucket_acl" {
  bucket = aws_s3_bucket.binplz_bucket.id
  acl    = "private"
}

variable "binplz_ami_path" {
  description = "Path to the binplz server ami image"
  type        = string
}

variable "binplz_image_name_hash" {
  description = "Name of the image"
  type        = string
}

resource "aws_s3_object" "image_upload" {
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

# https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/ebs_snapshot_import

# https://docs.aws.amazon.com/vm-import/latest/userguide/required-permissions.html#vmimport-role
# TODO KMS?
# TODO License Configurations?
# TODO Context Keys?
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
            "sts:Externalid" = "vmimport" # TODO pull from variable
          }
        }
      }
    ]
  })
}

resource "aws_iam_policy_attachment" "vmimport_attach" {
  name       = "vmimport_attach"
  roles      = [aws_iam_role.vmimport_role.id]
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
