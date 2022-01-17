import React from "react"
import {NavItem, NavLink} from "reactstrap"
import type {Tab} from "../../datatypes/Navigation"

export type TabViewProps = {
	name: Tab;
	activeTabName: string;
	onClick: (tab: Tab) => void;
	clickable: boolean;
};

export default class TabView extends React.Component<TabViewProps> {

	render(): JSX.Element {
		const isActive = this.props.activeTabName === this.props.name
		return <NavItem>
			<NavLink className={isActive ? "active" : this.props.clickable ? "clickable" : "not-clickable"}
					 onClick={(): void => {
						 if (!isActive) {
							 this.props.onClick(this.props.name)
						 }
					 }}>
				{this.props.name}
			</NavLink>
		</NavItem>
	}

}