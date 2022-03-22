import {store} from "../Store"
import {vernieuwAfsprakenDaglijst} from "../restclient/DaglijstRestclient"
import {createActionQueueDaglijstVerversen} from "../actions/UpdateAction"
import type {Transaction} from "../datatypes/Transaction"
import {dispatchActions} from "./DispatchUtil"
import {createActionOffline, createActionOnline} from "../actions/ConnectionActions"
import {persistentErrorToast, persistentSuccessToast} from "./ToastUtil"
import {nuISO, setOffset} from "./DateUtil"
import * as moment from "moment"
import {Duration} from "moment"
import {fetchApiPromise} from "./ApiUtil"
import {EnvironmentInfo} from "../datatypes/EnvironmentInfo"
import {Client, Message, Stomp} from "@stomp/stompjs"

export const initWebSocket = (): void => {
	let websockerUrl = `${(window.location.protocol === "https:" ? "wss:
	console.log(`Websocket aanmelden bij proxy endpoint: ${websockerUrl}`)

	if (process.env.NODE_ENV === "development") {
		websockerUrl = "ws:
	}

	const client: Client = Stomp.client(websockerUrl)
	client.debug = (): void => {
	}
	client.reconnectDelay = 1000
	client.onWebSocketClose = (e): void => {
		console.log("Verbinding maken met websocket mislukt, opnieuw proberen in 1000ms...")
		console.log(`WS onWebSocketClose ${JSON.stringify(e)}`)
	}
	client.onWebSocketError = (e): void => {
		console.log(`WS onWebSocketError ${JSON.stringify(e)}`)
	}
	client.onStompError = (e): void => {
		console.log(`WS onStompError ${JSON.stringify(e)}`)
	}
	client.onUnhandledFrame = (e): void => {
		console.log(`WS onUnhandledFrame ${JSON.stringify(e)}`)
	}
	client.onUnhandledMessage = (message): void => {
		console.log(`WS onUnhandledMessage ${JSON.stringify(message)}`)
	}
	client.onDisconnect = (e): void => {
		console.log(`WS onDisconnect ${JSON.stringify(e)}`)
	}
	client.onConnect = (e): void => {
		console.log(`WS onConnect ${JSON.stringify(e)}`)
		client.subscribe("/transactionReceive", (message: Message) => {
			try {
				verwerkSocketBericht(message)
			} catch (e: any) {
				console.error(`Fout bij het verwerken van socketbericht ${message} door ${e} ${e.stack && `stacktrace: ${e.stack}`}`)
			}
		})
	}

	client.activate()
	forceerUpdateVoorVerkeerdeVersie()
}

const forceerUpdateVoorVerkeerdeVersie = (): void => {
	const prevEnvironmentInfo = store.getState().environmentInfo
	const prevVersion = prevEnvironmentInfo && prevEnvironmentInfo.version
	fetchApiPromise("GET", "environmentInfo").then(response => {
		response.json().then((environmentInfo: EnvironmentInfo) => {
			if (environmentInfo.version && prevVersion && environmentInfo.version !== prevVersion) {
				window.onbeforeunload = function (): boolean | undefined {
					return undefined
				}

				window.location.reload()
				persistentSuccessToast("De SE is geüpdatet naar de nieuwste versie.")
			}
		})
	})
}

const DAGLIJST_UPDATE = "DAGLIJST_UPDATE"
const TIJD_UPDATE = "TIJD_UPDATE"
const ONLINE = "ONLINE"
const OFFLINE = "OFFLINE"
const SERVER_ERROR = "SERVER_ERROR"

const verwerkSocketBericht = (message: Message): void => {
	const body = message.body
	const command = body.split("###")[0]

	switch (command) {
		case ONLINE:
			dispatchActions(store.dispatch, createActionOnline())
			break
		case OFFLINE:
			dispatchActions(store.dispatch, createActionOffline())
			break
		case DAGLIJST_UPDATE:
			updateDaglijst()
			break
		case TIJD_UPDATE:
			const duration: string = body.split("###")[1]
			const momentDuration: Duration = moment.duration(duration)
			setOffset(momentDuration)
			console.log(`De tijd is verzet naar: ${nuISO()}`)
			break
		case SERVER_ERROR:
			persistentErrorToast(body.split("###")[1])
			break
		default:
			const receivedTransaction: Transaction = JSON.parse(body)
			dispatchActions(store.dispatch, ...receivedTransaction.actions)
	}
}

const updateDaglijst = (): void => {
	if (store.getState().navigation.tab === "Daglijst") {
		if (!store.getState().formsByFormId.get("passant_afspraak_maken")?.isSubmitted) {
			vernieuwAfsprakenDaglijst()
		}
	} else {
		store.dispatch(createActionQueueDaglijstVerversen())
	}
}
