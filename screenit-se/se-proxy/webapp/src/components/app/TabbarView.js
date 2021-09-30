/*-
 * ========================LICENSE_START=================================
 * se-proxy
 * %%
 * Copyright (C) 2017 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import React from 'react';
import {Nav, Spinner, TabContent, TabPane} from 'reactstrap';
import ClientgegevensContainer from '../clientgegevens/ClientgegevensContainer';
import DaglijstView from '../daglijst/DaglijstView';
import TabView from '../generic/TabView';
import type {SubPagina, Tab} from '../../datatypes/Navigation';
import {isClientgegevensTab, isConnectiestatusTab, isDaglijstTab, isDagverslagTab, isKwaliteitsopnameTab, isOnderzoeksTab} from '../../datatypes/Navigation';
import OnderzoekContainer from '../onderzoek/OnderzoekContainer';
import type {Afspraak} from '../../datatypes/Afspraak';
import ErrorView from './ErrorView';
import type {ErrorDto} from '../../datatypes/ErrorDto';
import DagverslagContainer from '../dagverslag/DagverslagContainer';
import AkkoordPopupContainer from '../generic/popup/AkkoordPopupContainer';
import KwaliteitsopnameContainer from '../kwaliteitsopname/KwaliteitsopnameContainer';
import ConnectieStatusContainer from '../connectiestatus/ConnectieStatusContainer';

type TabbarProps = {
    activeTab: Tab,
    afspraak: Afspraak,
    daglijstTabClickable: boolean,
    clientgegevensTabClickable: boolean,
    onderzoekTabClickable: boolean,
    dagverslagTabClickable: boolean,
    kwaliteitsopnameTabClickable: boolean,
    connectiestatusTabClickable: boolean,
    error: ErrorDto,
    magOnderzoeken: boolean,
    huidigeMammograafId: ?boolean,
    bezigMetKwaliteitsopname: boolean,
    subPagina: ?SubPagina,
    klaarMetDataLaden: boolean,

    onClickTab: (tab: Tab, subPagina: ?SubPagina, afspraak: Afspraak, magOnderzoeken: boolean) => mixed
}

export default class TabbarView extends React.Component<TabbarProps> {

    constructor(props: TabbarProps) {
        super(props);
        this.props = props;
    }

    render() {
        const activeTab = this.props.activeTab;
        if (!this.props.klaarMetDataLaden && (activeTab !== 'Connectiestatus')) {
            return <div>
                <Spinner className={'login-spinner'}/>
                <div className={'jumbotron text-center'}>Bezig met het laden van de daglijst</div>
            </div>;
        }
        const clickTab = (tab: Tab) => this.props.onClickTab(tab, this.props.subPagina, this.props.afspraak, this.props.magOnderzoeken);
        return (
            this.props.error ? <ErrorView error={this.props.error}/> :
                <div>
                    <AkkoordPopupContainer/>
                    <Nav tabs>
                        <TabView name={'Daglijst'} activeTabName={activeTab.toString()} onClick={clickTab} clickable={this.props.daglijstTabClickable}/>
                        <TabView name={'Cliëntgegevens'} activeTabName={activeTab.toString()} onClick={clickTab} clickable={this.props.clientgegevensTabClickable}/>
                        {this.props.magOnderzoeken ?
                            <TabView name={'Onderzoek'} activeTabName={activeTab.toString()} onClick={clickTab} clickable={this.props.onderzoekTabClickable}/> :
                            null}
                        <TabView name={'Dagverslag'} activeTabName={activeTab.toString()} onClick={clickTab} clickable={this.props.dagverslagTabClickable}/>
                        <TabView name={'Kwaliteitsopname'} activeTabName={activeTab.toString()} onClick={clickTab} clickable={this.props.kwaliteitsopnameTabClickable}/>
                        <TabView name={'Connectiestatus'} activeTabName={activeTab.toString()} onClick={clickTab} clickable={this.props.connectiestatusTabClickable}/>
                    </Nav>
                    <TabContent activeTab={this.props.activeTab}>
                        <TabPane tabId="Daglijst">
                            {isDaglijstTab(this.props.activeTab) ? <DaglijstView/> : null}
                        </TabPane>
                        <TabPane tabId="Cliëntgegevens">
                            {isClientgegevensTab(this.props.activeTab) ? <ClientgegevensContainer/> : null}
                        </TabPane>
                        <TabPane tabId="Onderzoek">
                            {isOnderzoeksTab(this.props.activeTab) ? <OnderzoekContainer/> : null}
                        </TabPane>
                        <TabPane tabId="Dagverslag">
                            {isDagverslagTab(this.props.activeTab) ? <DagverslagContainer/> : null}
                        </TabPane>
                        <TabPane tabId="Kwaliteitsopname">
                            {isKwaliteitsopnameTab(this.props.activeTab) ? <KwaliteitsopnameContainer/> : null}
                        </TabPane>
                        <TabPane tabId="Connectiestatus">
                            {isConnectiestatusTab(this.props.activeTab) ? <ConnectieStatusContainer/> : null}
                        </TabPane>
                    </TabContent>
                </div>
        );
    }
}
