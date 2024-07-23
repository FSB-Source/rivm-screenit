<<<<<<<< HEAD:screenit-batch-dk/src/main/java/nl/rivm/screenit/batch/service/ColonFITHL7BerichtInlezenService.java
package nl.rivm.screenit.batch.service;
========
package nl.rivm.screenit.dto.mamma;
>>>>>>>> refs/heads/main:screenit-base/src/main/java/nl/rivm/screenit/dto/mamma/MammaDense2ConfiguratieDto.java

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

import java.util.List;

<<<<<<<< HEAD:screenit-batch-dk/src/main/java/nl/rivm/screenit/batch/service/ColonFITHL7BerichtInlezenService.java
import nl.rivm.screenit.model.colon.berichten.ColonIFobtUitslagBericht;
import nl.rivm.screenit.model.logging.IfobtVerwerkingBeeindigdLogEvent;

public interface ColonFITHL7BerichtInlezenService
{
	List<ColonIFobtUitslagBericht> getAlleNietVerwerkteFitBerichten();

	void verwerkOntvangenFitBericht(ColonIFobtUitslagBericht bericht);

	void logError(ColonIFobtUitslagBericht ontvangenBericht, String message, IfobtVerwerkingBeeindigdLogEvent verwerkingLogEvent);
========
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class MammaDense2ConfiguratieDto
{
	private String cem;

	private String mri;

	private String controleGroep;

	private String herinneringCem;

	private String herinneringMri;

	private boolean metingOpslaan;

	private String exportBestandsnaam;

	private List<Long> excludeProjecten;

	public List<String> getDenseProjecten()
	{
		return List.of(cem, mri, controleGroep, herinneringCem, herinneringMri);
	}

	public List<String> getDenseOnderzoekProjecten()
	{
		return List.of(cem, mri);
	}
}
