package nl.rivm.screenit.service.cervix;

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

import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixLabformulierenFilter;
import nl.rivm.screenit.model.cervix.CervixMonster;

public interface CervixLabformulierService
{
	List<CervixLabformulier> getLabformulieren(CervixLabformulierenFilter filter, long first, long count, String sortProperty, boolean asc);

	int countLabformulieren(CervixLabformulierenFilter filter);

	List<Long> getLabformulierenIds(CervixLabformulierenFilter filter, String sortProperty, boolean asc);

	void valideerLabformulier(CervixLabformulier labformulier);

	String koppelEnBewaarLabformulier(CervixLabformulier labformulier);

	void koppelDigitaalLabformulier(CervixLabformulier labformulier);

	void updateLabformulierLaboratoriumNaOntvangstMonster(CervixMonster monster);
}
