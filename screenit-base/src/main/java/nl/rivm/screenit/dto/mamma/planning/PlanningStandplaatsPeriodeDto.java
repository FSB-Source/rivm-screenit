package nl.rivm.screenit.dto.mamma.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

public class PlanningStandplaatsPeriodeDto extends PlanningConceptEntiteitDto
{
	public Long id;

	public Long standplaatsId;

	public Long achtervangStandplaatsId;

	public Long minderValideUitwijkStandplaatsId;

	public List<Long> afspraakcapaciteitBeschikbaarVoorIds = new ArrayList<>();

	public Integer screeningsEenheidVolgNr;

	public Integer standplaatsRondeVolgNr;

	public Integer afspraakDrempel;

	@JsonDeserialize(using = LocalDateDeserializer.class)
	@JsonSerialize(using = LocalDateSerializer.class)
	public LocalDate vanaf;

	@JsonDeserialize(using = LocalDateDeserializer.class)
	@JsonSerialize(using = LocalDateSerializer.class)
	public LocalDate totEnMet;

	public BigDecimal initieelIntervalMaanden;

	public BigDecimal intervalMaanden;

	public List<Long> blokkadeIds;

	public Boolean prognose;

	public PlanningMeldingenDto meldingenDto;

	public Boolean gesplitst;

	@JsonDeserialize(using = LocalDateDeserializer.class)
	@JsonSerialize(using = LocalDateSerializer.class)
	public LocalDate minderValideUitnodigenVanaf;
}
