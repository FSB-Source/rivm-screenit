package nl.rivm.screenit.specification.mamma;

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

import java.time.LocalDate;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Root;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaOnderzoekSpecification
{
	public static Specification<Client> heeftClientOnderzoekAangemaaktVanaf(LocalDate datum)
	{
		return (r, q, cb) -> cb.greaterThanOrEqualTo(onderzoekJoin(r).get(MammaOnderzoek_.creatieDatum), DateUtil.toUtilDate(datum));
	}

	public static Specification<Client> heeftClientLaatsteOnderzoekVolledig()
	{
		return (r, q, cb) -> cb.equal(onderzoekJoin(r).get(MammaOnderzoek_.status), MammaOnderzoekStatus.AFGEROND);
	}

	private static From<MammaScreeningRonde, MammaOnderzoek> onderzoekJoin(Root<Client> clientRoot)
	{
		var dossierJoin = SpecificationUtil.join(clientRoot, Client_.mammaDossier);
		var rondeJoin = SpecificationUtil.join(dossierJoin, MammaDossier_.laatsteScreeningRonde);
		return SpecificationUtil.join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
	}
}
