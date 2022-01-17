import React from "react"
import AppContainer from "./AppContainer"
import FooterContainer from "./FooterContainer"
import HeaderContainer from "./HeaderContainer"
import ErrorBoundary from "./ErrorBoundary"
import DubbeleInstantieDetector from "./DubbeleInstantieDetector"

const BuitensteView = (): JSX.Element => {
	return <ErrorBoundary>
		<div className="buitenste-view">
			<div className="app-header">
				<img className="screenit-appnaam" src="images/screenit_appnaam.png" alt="ScreenIT"/>
				<HeaderContainer/>
			</div>
			<DubbeleInstantieDetector/>
			<AppContainer/>
			<FooterContainer/>
		</div>
	</ErrorBoundary>
}

export default BuitensteView