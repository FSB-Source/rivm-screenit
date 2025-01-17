package nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken;

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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaCapaciteitBlok;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.search.column.HibernateCheckBoxListContainer;

import org.apache.wicket.AttributeModifier;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class MammaBlokMetAfsprakenPanel extends GenericPanel<MammaCapaciteitBlok>
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private HibernateService hibernateService;

	public MammaBlokMetAfsprakenPanel(String id, IModel<MammaCapaciteitBlok> model, HibernateCheckBoxListContainer<MammaAfspraak> selectedAfspraken, LocalDate currentDay,
		BootstrapDialog dialog, boolean magVerzetten, boolean magBulkVerzetten)
	{
		super(id, model);

		MammaCapaciteitBlok capaciteitBlok = model.getObject();
		hibernateService.reload(capaciteitBlok);

		ScreeningOrganisatie screeningOrganisatie = (ScreeningOrganisatie) HibernateHelper
			.deproxy(capaciteitBlok.getScreeningsEenheid().getBeoordelingsEenheid().getParent().getRegio());
		MammaCapaciteitBlokType blokType = capaciteitBlok.getBlokType();
		BigDecimal factor = blokType.getFactorType().getFactor(screeningOrganisatie);

		BigDecimal vrijeCapaciteitRegulier = capaciteitBlok.getVrijeCapaciteit();
		BigDecimal beschikbareCapaciteitRegulier = capaciteitBlok.getBeschikbareCapaciteit();
		BigDecimal vrijeCapaciteit = vrijeCapaciteitRegulier.divide(factor, 5, RoundingMode.HALF_UP);
		BigDecimal beschikbareCapaciteit = beschikbareCapaciteitRegulier.divide(factor, 5, RoundingMode.HALF_UP);

		WebMarkupContainer blok = new WebMarkupContainer("blok");
		add(blok);

		blok.add(new EnumLabel<MammaCapaciteitBlokType>("blokType"));

		blok.add(DateLabel.forDatePattern("vanaf", "HH:mm"));
		blok.add(DateLabel.forDatePattern("tot", "HH:mm"));

		blok.add(new Label("vrijeCapaciteit", vrijeCapaciteit.setScale(1, RoundingMode.HALF_UP).toString()));
		blok.add(new Label("beschikbareCapaciteit", BigDecimalUtil.decimalToString(beschikbareCapaciteit, 1)));

		WebMarkupContainer regulier = new WebMarkupContainer("regulier");
		regulier.setVisible(blokType != MammaCapaciteitBlokType.REGULIER);
		blok.add(regulier);
		regulier.add(new Label("vrijeCapaciteitRegulier", vrijeCapaciteitRegulier.setScale(1, RoundingMode.HALF_UP).toString()));
		regulier.add(new Label("beschikbareCapaciteitRegulier", BigDecimalUtil.decimalToString(beschikbareCapaciteitRegulier, 1)));

		List<MammaAfspraak> afspraken = capaciteitBlok.getAfspraken().stream().filter(mammaAfspraak -> mammaAfspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND))
			.sorted(Comparator.comparing(MammaAfspraak::getVanaf)).collect(Collectors.toList());
		magBulkVerzetten &= !blokType.equals(MammaCapaciteitBlokType.TEHUIS);
		blok.add(new MammaAfsprakenBlokPanel("afspraken", ModelUtil.listRModel(afspraken), selectedAfspraken, currentDay, magVerzetten, magBulkVerzetten));
		blok.add(new AttributeModifier("class", " mamma-blok-met-afspraken-" + blokType.name().toLowerCase()));
	}
}
