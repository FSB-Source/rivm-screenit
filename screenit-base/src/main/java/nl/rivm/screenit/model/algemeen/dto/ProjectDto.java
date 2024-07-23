<<<<<<<< HEAD:screenit-web/src/main/java/nl/rivm/screenit/main/web/gebruiker/screening/mamma/exchange/followup/MammaFollowUpRadiologieInstellingOptieFilter.java
package nl.rivm.screenit.main.web.gebruiker.screening.mamma.exchange.followup;
========
package nl.rivm.screenit.model.algemeen.dto;
>>>>>>>> refs/heads/main:screenit-base/src/main/java/nl/rivm/screenit/model/algemeen/dto/ProjectDto.java

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

<<<<<<<< HEAD:screenit-web/src/main/java/nl/rivm/screenit/main/web/gebruiker/screening/mamma/exchange/followup/MammaFollowUpRadiologieInstellingOptieFilter.java
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.MammaFollowUpDoorverwezenFilterOptie;

@Getter
@Setter
@AllArgsConstructor
public class MammaFollowUpRadiologieInstellingOptieFilter
{
	MammaFollowUpDoorverwezenFilterOptie mammaFollowUpDoorverwezenFilterOptie;

	Instelling instelling;
========
import java.io.Serializable;
import java.time.LocalDate;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ProjectDto implements Serializable
{
	private String naam;

	private LocalDate startDatum;

	private LocalDate eindDatum;

	private Long id;
>>>>>>>> refs/heads/main:screenit-base/src/main/java/nl/rivm/screenit/model/algemeen/dto/ProjectDto.java
}
