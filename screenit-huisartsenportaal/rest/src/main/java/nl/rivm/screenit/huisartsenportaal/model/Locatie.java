package nl.rivm.screenit.huisartsenportaal.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.huisartsenportaal.ICervixHuisartsLocatie;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;

import org.hibernate.envers.Audited;

@Entity
@Audited
@Getter
@Setter
public class Locatie extends AbstractReferenceObject implements ICervixHuisartsLocatie
{
	@Column(length = 200, nullable = false)
	private String naam;

	@OneToOne(fetch = FetchType.EAGER)
	private Adres locatieAdres;

	@Column(length = 34, nullable = false)
	private String iban;

	@Column(length = 70, nullable = false)
	private String ibanTenaamstelling;

	@OneToMany(mappedBy = "locatie", fetch = FetchType.LAZY)
	public List<LabformulierAanvraag> aanvragen;

	@Column(nullable = false)
	private String zorgmailklantnummer;

	@ManyToOne(fetch = FetchType.LAZY)
	public Huisarts huisarts;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private CervixLocatieStatus status;

	@Column(length = 255, nullable = true, unique = false)
	private String verificatieCode;

	private Boolean moetVerifierenVoorActivatie = false;

}
