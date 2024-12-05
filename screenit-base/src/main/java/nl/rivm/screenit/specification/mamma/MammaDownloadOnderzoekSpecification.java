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

import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker_;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoekenVerzoek_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = lombok.AccessLevel.PRIVATE)
public class MammaDownloadOnderzoekSpecification
{
	public static Specification<MammaDownloadOnderzoek> heeftDossier(MammaDossier dossier)
	{
		return MammaScreeningRondeSpecification.heeftDossier(dossier).with(r ->
		{
			var onderzoekJoin = join(r, MammaDownloadOnderzoek_.onderzoek);
			var afspraakJoin = join(onderzoekJoin, MammaOnderzoek_.afspraak);
			var uitnodigingJoin = join(afspraakJoin, MammaAfspraak_.uitnodiging);
			return join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
		});
	}

	public static Specification<MammaDownloadOnderzoek> isGemaaktDoorActieveInstelling()
	{
		return (r, q, cb) ->
		{
			var verzoekJoin = join(r, MammaDownloadOnderzoek_.verzoek);
			var aangemaaktDoorJoin = join(verzoekJoin, MammaDownloadOnderzoekenVerzoek_.aangemaaktDoor);
			return cb.isTrue(cb.treat(aangemaaktDoorJoin.get(InstellingGebruiker_.organisatie), Instelling.class).get(Instelling_.actief));
		};
	}

	public static ExtendedSpecification<MammaDownloadOnderzoek> isGedownload()
	{
		return (r, q, cb) -> cb.isNotNull(join(r, MammaDownloadOnderzoek_.verzoek).get(MammaDownloadOnderzoekenVerzoek_.gedownloadOp));
	}
}
