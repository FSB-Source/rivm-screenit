package nl.rivm.screenit.specification.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDate;
import java.util.List;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.specification.algemeen.OrganisatieMedewerkerSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.ExtendedSpecification.not;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNullExtended;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaDownloadOnderzoekenVerzoekSpecification
{
	public static Specification<MammaDownloadOnderzoekenVerzoek> heeftScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		return (r, q, cb) ->
		{
			var downloadOnderzoekJoin = SpecificationUtil.join(r, MammaDownloadOnderzoekenVerzoek_.onderzoeken);
			var onderzoekJoin = SpecificationUtil.join(downloadOnderzoekJoin, MammaDownloadOnderzoek_.onderzoek);
			var afspraakJoin = SpecificationUtil.join(onderzoekJoin, MammaOnderzoek_.afspraak);
			var uitnodigingJoin = SpecificationUtil.join(afspraakJoin, MammaAfspraak_.uitnodiging);
			return cb.equal(uitnodigingJoin.get(MammaUitnodiging_.screeningRonde), screeningRonde);
		};
	}

	public static ExtendedSpecification<MammaDownloadOnderzoekenVerzoek> isAangemaaktDoorGebruikerVanInstelling(Instelling instelling)
	{
		return OrganisatieMedewerkerSpecification.heeftInstelling(instelling).with(MammaDownloadOnderzoekenVerzoek_.aangemaaktDoor);
	}

	public static ExtendedSpecification<MammaDownloadOnderzoekenVerzoek> filterOpAangemaaktDoorGebruikerVanInstelling(Instelling instelling)
	{
		return skipWhenNullExtended(instelling, isAangemaaktDoorGebruikerVanInstelling(instelling));
	}

	public static ExtendedSpecification<MammaDownloadOnderzoekenVerzoek> heeftGeenStatusIn(List<BestandStatus> bestandStatussen)
	{
		return (r, q, cb) -> cb.not(r.get(MammaDownloadOnderzoekenVerzoek_.status).in(bestandStatussen));
	}

	public static ExtendedSpecification<MammaDownloadOnderzoekenVerzoek> heeftStatus(BestandStatus bestandStatus)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaDownloadOnderzoekenVerzoek_.status), bestandStatus);
	}

	public static ExtendedSpecification<MammaDownloadOnderzoekenVerzoek> heeftNietStatus(BestandStatus bestandStatus)
	{
		return not(heeftStatus(bestandStatus));
	}

	public static ExtendedSpecification<MammaDownloadOnderzoekenVerzoek> isAangemaaktOpVoor(LocalDate datum)
	{

		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(MammaDownloadOnderzoekenVerzoek_.aangemaaktOp), DateUtil.toUtilDate(datum));
	}

}
