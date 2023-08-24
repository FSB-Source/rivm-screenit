/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import React from "react";

type CheckmarkIconComponentProps = {
    onClick: () => void,
    checked: boolean,
}

const CheckmarkIconComponent = (props: CheckmarkIconComponentProps) => {
    return <svg xmlns="http:
        <path id="Path"
              d="M11.345.175a.833.833,0,0,1,.146,1.169L5.012,9.678a.833.833,0,0,1-1.231.094L.26,6.439a.833.833,0,0,1,1.146-1.21L4.26,7.93,10.175.322A.833.833,0,0,1,11.345.175Z"
              transform="translate(4.167 5)" fill={props.checked ? "#fff" : "transparent"}/>
    </svg>
}

export default CheckmarkIconComponent
