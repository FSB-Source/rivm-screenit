import React from "react"
import type {MammograafDicomMessageError, MammograafDicomMessageType} from "../../datatypes/connectiestatus/MammograafDicomMessageError"
import moment from "moment"

export type MammograafDicomFoutmeldingenPopupViewProps = {
	messageType: MammograafDicomMessageType;
	errors: Array<MammograafDicomMessageError>;
};

export default class MammograafDicomFoutmeldingenPopupView extends React.Component<MammograafDicomFoutmeldingenPopupViewProps> {
	render(): JSX.Element {
		return <div>
			{this.props.messageType === "MPPS" ? <div>
				{this.props.errors.reverse().map((e: MammograafDicomMessageError) => {
					return <p key={e.timestamp} className={"mammograafDicomMessageErrorText"}>
						<b>{moment(e.timestamp).fromNow()}</b>: {e.message}</p>
				})}
			</div> : <div>
				{this.props.errors.reverse().map((e: MammograafDicomMessageError) => {
					return <p key={e.timestamp} className={"mammograafDicomMessageErrorText"}>
						<b>{moment(e.timestamp).fromNow()}</b>: DMWL mislukt door mismatch in keys. {e.message}</p>
				})}
			</div>}
		</div>
	}

}