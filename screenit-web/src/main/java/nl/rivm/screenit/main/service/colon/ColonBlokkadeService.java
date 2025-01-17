package nl.rivm.screenit.main.service.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import javax.annotation.Nullable;

import nl.rivm.screenit.exceptions.OpslaanVerwijderenTijdBlokException;
import nl.rivm.screenit.main.exception.BulkAanmakenException;
import nl.rivm.screenit.main.exception.BulkVerwijderenException;
import nl.rivm.screenit.main.exception.ValidatieException;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.RoosterListViewFilter;
import nl.rivm.screenit.model.colon.dto.ColonBlokkadeDto;
import nl.rivm.screenit.model.colon.dto.ColonHerhalingDto;
import nl.rivm.screenit.model.colon.dto.ColonTijdslotDto;
import nl.rivm.screenit.model.colon.planning.ColonBlokkade;
import nl.rivm.screenit.model.colon.planning.ColonIntakekamer;
import nl.rivm.screenit.model.enums.LogGebeurtenis;

import org.springframework.data.domain.Sort;

import com.google.common.collect.Range;

public interface ColonBlokkadeService
{
	void createBlokkade(ColonBlokkadeDto blokkadeDto, InstellingGebruiker instellingGebruiker)
		throws ValidatieException, OpslaanVerwijderenTijdBlokException, BulkAanmakenException;

	void logAction(ColonBlokkade unsavedObject, InstellingGebruiker instellingGebruiker, ColonIntakelocatie intakelocatie, @Nullable ColonBlokkade origineleBlokkade,
		LogGebeurtenis logGebeurtenis, ColonHerhalingDto herhalingDto, Exception ex);

	String getPeriodeTekst(ColonBlokkade unsavedObject);

	void deleteBlokkade(Long blokkadeId, InstellingGebruiker instellingGebruiker) throws ValidatieException;

	Optional<ColonBlokkade> getBlokkade(Long blokkadeId);

	void updateBlokkade(ColonBlokkadeDto blokkadeDto, InstellingGebruiker loggedInInstellingGebruiker) throws OpslaanVerwijderenTijdBlokException, ValidatieException;

	List<ColonTijdslotDto> zoekBlokkades(RoosterListViewFilter filter, long intakelocatieId);

	void bulkDeleteBlokkades(List<Long> blokkadeIds, InstellingGebruiker loggedInInstellingGebruiker, boolean alleenValidatie) throws BulkVerwijderenException;

	List<ColonBlokkade> zoekBlokkadesInRange(Range<LocalDateTime> range);

	List<ColonBlokkade> getBlokkades(ColonIntakekamer kamer, LocalDateTime vanaf, LocalDateTime tot);

	List<ColonBlokkade> getBlokkades(Sort sort, long first, long count, RoosterListViewFilter filter, ColonIntakelocatie intakelocatie);

	long getBlokkadesCount(RoosterListViewFilter filter, ColonIntakelocatie intakelocatie);
}
