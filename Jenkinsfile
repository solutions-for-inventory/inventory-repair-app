pipeline {
    agent any
    environment {
        PATH = "/usr/local/bin:$PATH"
        GENERATE_SOURCEMAP = "false"
      }

    stages {
        stage('CleanOldBinary') {
            when { branch 'release-1.0' }
            steps {
               catchError {
                 sh 'rm -rf webapps/dist'
//                 sh 'rm -rf .stack-work'
                 sh 'docker stop inventory-repair-app'
                 sh 'docker rm inventory-repair-app'
//                 sh 'docker images -a | grep "inventory-repair-app" | awk \'{print $3}\' | xargs docker rmi'
               }
            }
        }
        stage('Build') {
            steps {
                sh 'mkdir -p webapps/dist'
                sh 'stack build --copy-bins --local-bin-path target'
            }
        }
        stage('DockerBuildImage') {
//            when { branch 'release-1.0' }
            steps {
                echo 'Starting to build docker image'
                script {
                    def customImage = docker.build("inventory-repair-app:1.0")
                }
            }
        }
        stage('Test') {
            steps {
                echo 'Testing..'
            }
        }
        stage('Deploy') {
            when { branch 'release-1.0' }
            steps {
                echo 'Deploying....'
                script {
                    docker.image("inventory-repair-app:1.0")
                    .run('--name inventory-repair-app --net=host '
                        + '-e YESOD_PORT=3000 '
                        + '-e YESOD_PGUSER=inventory_user '
                        + '-e YESOD_PGPASS=inventory_password '
                        + '-e YESOD_PGHOST=192.168.0.100 '
                        + '-e OAUTH2_CLIENT_ID=app '
                        + '-e OAUTH2_SECRET=appsecret '
                        + '-e OAUTH2_AUTHORIZE=http://192.168.0.100:3001/oauth/authorize '
                        + '-e OAUTH2_ACCESS_TOKEN=http://192.168.0.100:3001/oauth/token '
                        + '-e OAUTH2_USER_INFO=http://192.168.0.100:3001/connect/userinfo '
                        + '-e OAUTH2_LOGOUT=http://192.168.0.100:3001/logout '
                        + '-e OAUTH2_SCOPES=openid'
                    )
                }
            }
        }
    }
}
