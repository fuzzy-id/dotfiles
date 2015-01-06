GPG_AGENT_FILE="${HOME}/.gpg-agent"

function connect_to_running_instance () {
    if [[ ! -f "${GPG_AGENT_FILE}" ]]; then
	return 1
    fi
    source "${GPG_AGENT_FILE}"
    export GPG_AGENT_INFO
    gpg-agent --quiet 2> /dev/null
}

function start_new_instance () {
    eval $(gpg-agent --daemon --write-env-file "${GPG_AGENT_FILE}")
}

connect_to_running_instance || start_new_instance

eval $(ssh-agent)
