pipeline {
    agent any
    options { disableConcurrentBuilds() }
    environment {
        CUR_PROJ = 'tracer' // github repo name
        CUR_PKG_FOLDER = '.' // defaults to root
        TMP_SUFFIX = """${sh(returnStdout: true, script: 'echo `cat /dev/urandom | tr -dc \'a-z\' | fold -w 6 | head -n 1`')}"""
        GH_TOKEN = credentials("github-isomemo")
        PRIVATE_KEY = credentials('tracer_private_key')
        PUBLIC_KEY = credentials('tracer_public_key')
    }
    stages {
        stage('Testing') {
            steps {
                sh '''
                cp -f $PRIVATE_KEY inst/app/private_key.pem
                cp -f $PUBLIC_KEY inst/app/public_key.pem
                docker build --pull -t tmp-$CUR_PROJ-$TMP_SUFFIX .
                docker run --rm --network host tmp-$CUR_PROJ-$TMP_SUFFIX check
                docker rmi tmp-$CUR_PROJ-$TMP_SUFFIX
                rm -f inst/app/private_key.pem
                rm -f inst/app/public_key.pem
                '''
            }
        }

        stage('Deploy R-package') {
            when { branch 'main' }
            steps {
                sh '''
                curl https://raw.githubusercontent.com/Pandora-IsoMemo/drat/main/deploy.sh > deploy.sh
                # Expects environment variables:
                # CUR_PROJ
                # TMP_SUFFIX
                # GH_TOKEN_PSW -- a GitHub personal access token with write access to the drat repo

                bash deploy.sh
                '''
            }
        }
    }
}
