import React from "react"

export default class AfsprakenDoorvoerenView extends React.Component {
	render(): JSX.Element {
		return <div>
			<p>
				<b>Let op!</b><br/><br/>
				Alle onderzoeken met status afgerond/onvolledig/onderbroken kunnen niet meer worden bewerkt.<br/>
			</p>
		</div>
	}

}
