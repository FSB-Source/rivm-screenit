package nl.rivm.screenit.specification.algemeen;

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

import java.util.List;
import java.util.Optional;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Afmelding;
import nl.rivm.screenit.model.BagAdres;
import nl.rivm.screenit.model.BagAdres_;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.ClientBrief_;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.Gemeente_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.specification.ExtendedSpecification;

import static nl.rivm.screenit.model.algemeen.AlgemeneBrief_.MERGED_BRIEVEN;
import static nl.rivm.screenit.model.colon.ColonBrief_.AFMELDING;
import static nl.rivm.screenit.specification.HibernateObjectSpecification.heeftId;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.treat;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.heeftBriefTypeIn;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isGegenereerd;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isNietTegengehouden;
import static nl.rivm.screenit.specification.algemeen.BriefSpecification.isNietVervangen;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.isNietOverledenEnWoontInNederland;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ClientBriefSpecification
{
	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftClient(Client client)
	{
		return (r, q, cb) -> cb.equal(r.get(ClientBrief_.client), client);
	}

	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftGeenMergedBrieven(Class<? extends B> briefClass)
	{
		return (r, q, cb) -> cb.and(treat(r, briefClass, cb).get(MERGED_BRIEVEN).isNull(), cb.equal(r.type(), briefClass));
	}

	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftVervangendeProjectBrief(boolean heeftVervangendeProjectBrief)
	{
		return (r, q, cb) -> cb.equal(r.get(ClientBrief_.vervangendeProjectBrief), heeftVervangendeProjectBrief);
	}

	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftOngegeneerdeBrieven(BriefType type, Client client, Class<B> briefClass)
	{
		return heeftGeenMergedBrieven(briefClass).and(heeftClient(client))
			.and(isNietVervangen())
			.and(heeftBriefTypeIn(type.getMagNietOpZelfdeDagAfgedruktTypes()));
	}

	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftOngegeneerdeBrieven(Client client, Class<B> briefClass)
	{
		return heeftGeenMergedBrieven(briefClass).and(heeftClient(client))
			.and(isNietVervangen());
	}

	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftGegenereerdeBriefOfProjectBriefVanType(List<BriefType> briefTypes)
	{
		return ClientBriefSpecification.<B> heeftGegenereerdeReguliereBriefVanType(briefTypes)
			.or(heeftGegenereerdeProjectBriefVanType(briefTypes));
	}

	private static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftGegenereerdeReguliereBriefVanType(List<BriefType> briefTypes)
	{
		return ClientBriefSpecification.<B> heeftVervangendeProjectBrief(false)
			.and(heeftBriefTypeIn(briefTypes))
			.and(isGegenereerd(true));
	}

	private static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftGegenereerdeProjectBriefVanType(List<BriefType> briefTypes)
	{
		return ClientBriefSpecification.<B> heeftVervangendeProjectBrief(true)
			.and(heeftBriefTypeIn(briefTypes).with(r -> join(r, ClientBrief_.projectBrief, JoinType.LEFT)))
			.and(isGegenereerd(true).with(r1 -> join(r1, ClientBrief_.projectBrief, JoinType.LEFT)));
	}

	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftScreeningsOrganisatieId(Long screeningOrganisatieId)
	{
		return heeftId(screeningOrganisatieId).with(r -> join(gemeenteJoin(r), Gemeente_.screeningOrganisatie));
	}

	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftScreeningsOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return heeftScreeningsOrganisatieId(Optional.ofNullable(screeningOrganisatie).map(ScreeningOrganisatie::getId).orElse(null));
	}

	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> isClientGekoppeldAanEenScreeningOrganisatie()
	{
		return GemeenteSpecification.heeftScreeningOrganisatie().with(ClientBriefSpecification::gemeenteJoin);
	}

	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> heeftAfmelding(Afmelding afmelding, Class<? extends B> briefClass)
	{
		return (r, q, cb) -> cb.equal(treat(r, briefClass, cb).get(AFMELDING), afmelding);
	}

	private static <B extends ClientBrief<?, ?, ?>> Join<BagAdres, Gemeente> gemeenteJoin(From<?, ? extends B> r)
	{
		var adresJoin = join(persoonJoin(r), GbaPersoon_.gbaAdres);
		return join(adresJoin, BagAdres_.gbaGemeente);
	}

	private static <B extends ClientBrief<?, ?, ?>> Join<Client, GbaPersoon> persoonJoin(From<?, ? extends B> r)
	{
		var clientJoin = join(r, ClientBrief_.client);
		return join(clientJoin, Client_.persoon);
	}

	public static <B extends ClientBrief<?, ?, ?>> ExtendedSpecification<B> magGegenereerdWorden()
	{
		return BriefSpecification.<B> isNietGegenereerd()
			.and(isNietVervangen())
			.and(isNietTegengehouden())
			.and(heeftVervangendeProjectBrief(false))
			.and(isNietOverledenEnWoontInNederland().with(ClientBriefSpecification::persoonJoin));
	}
}
