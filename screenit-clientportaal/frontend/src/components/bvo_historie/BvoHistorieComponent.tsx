/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import React from "react"
import {Container} from "react-bootstrap"
import BvoHistorieCard from "./BvoHistorieCard"
import landingPageStyle from "../../pages/landing/LandingPage.module.scss"
import {isEmptyOrUndefined} from "../../utils/EmptyUtil"
import {ClientGebeurtenis} from "../../datatypes/ClientGebeurtenis"

type Props = {
    gebeurtenissen: ClientGebeurtenis[]
}

const BvoHistorieComponent = (props: Props) => {
    return (
        <div>
            {!isEmptyOrUndefined(props.gebeurtenissen) &&
            <h5 className={landingPageStyle.sectieHeader}>Mijn historie</h5>}
            <Container fluid>
                {props.gebeurtenissen.map((gebeurtenis, count) => <BvoHistorieCard
                        key={count} tekstKey={gebeurtenis.tekstKey} datumTijd={gebeurtenis.datumTijd}
                        extraParameters={gebeurtenis.extraParameters}/>)
                }
            </Container>
        </div>
    )
}

export default BvoHistorieComponent
