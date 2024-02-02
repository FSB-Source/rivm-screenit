package nl.rivm.screenit.model;

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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Setter
@Getter
@Audited
public class BMHKLaboratorium extends Instelling
{

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(schema = "algemeen", name = "user_id_scanners", uniqueConstraints = @UniqueConstraint(columnNames = { "userIdScanners" }))
	private List<String> userIdScanners = new ArrayList<>();

	@ElementCollection(fetch = FetchType.LAZY)
	@CollectionTable(schema = "algemeen", name = "instrument_names", uniqueConstraints = @UniqueConstraint(columnNames = { "instrumentNames" }))
	private List<String> instrumentNames = new ArrayList<>();

	@OneToMany(mappedBy = "bmhkLaboratorium", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<Gemeente> gemeentes = new ArrayList<>();

	@OneToMany(mappedBy = "laboratorium", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
	private List<ZASRetouradres> retouradressen = new ArrayList<>();

	@Column(length = 255)
	private String medischMircobioloog;

	@ManyToOne(fetch = FetchType.LAZY)
	private UploadDocument handtekeningMedischMircobioloog;

	@Column(length = 255)
	private String patholoog;

	@ManyToOne(fetch = FetchType.LAZY)
	private UploadDocument handtekeningPatholoog;

	@Column(length = 34, nullable = true)
	private String iban;

	@Column(length = 70, nullable = true)
	private String ibanTenaamstelling;

	@Column(length = 100, nullable = true)
	private String bmhkLabWarnMail;

	@Column(nullable = true)
	private Boolean oruBerichtenVerwerken = true;

}
