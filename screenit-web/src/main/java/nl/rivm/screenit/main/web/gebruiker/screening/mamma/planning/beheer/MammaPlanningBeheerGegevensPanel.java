package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.beheer;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.form.BigDecimalField;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.InstellingService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.HiddenField;
import org.apache.wicket.markup.html.form.NumberTextField;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;

public class MammaPlanningBeheerGegevensPanel extends GenericPanel<ScreeningOrganisatie>
{
	@SpringBean(name = "applicationUrl")
	private String applicationUrl;

	@SpringBean
	private InstellingService instellingService;

	private boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING_BEHEER, Actie.AANPASSEN)
		&& ScreenitSession.get().getScreeningOrganisatie() != null;

	public MammaPlanningBeheerGegevensPanel(String id, IModel<ScreeningOrganisatie> model)
	{
		super(id, model);
		setOutputMarkupId(true);

		ScreenitForm<ScreeningOrganisatie> form = new ScreenitForm<>("form", model);

		TextField<Integer> afspraakDrempelBk = new TextField<>("afspraakDrempelBk");
		afspraakDrempelBk.setType(Integer.class);
		afspraakDrempelBk.add(RangeValidator.range(0, 100));
		afspraakDrempelBk.setRequired(true);
		afspraakDrempelBk.setEnabled(magAanpassen);
		form.add(afspraakDrempelBk);
		form.add(new BigDecimalField("factorDubbeleTijdBk", 2, BigDecimal.ZERO, new BigDecimal("4.0")).setRequired(true).setEnabled(magAanpassen));
		form.add(new BigDecimalField("factorMinderValideBk", 2, BigDecimal.ZERO, new BigDecimal("4.0")).setRequired(true).setEnabled(magAanpassen));
		form.add(new BigDecimalField("factorEersteOnderzoekBk", 2, BigDecimal.ZERO, new BigDecimal("4.0")).setRequired(true).setEnabled(magAanpassen));
		form.add(new NumberTextField<Integer>("wekenVanTevorenUitnodigen").setRequired(true).setEnabled(magAanpassen));
		form.add(new NumberTextField<Integer>("vervallenCapaciteitsreserveringDagenBk").setMinimum(0).setRequired(true).setEnabled(magAanpassen));
		form.add(new NumberTextField<Integer>("minimaleDagCapaciteitMinderValideAfspraken").setRequired(true).setEnabled(magAanpassen));
		form.add(new HiddenField("applicationUrl", Model.of(applicationUrl)));
		form.add(new HiddenField("subUrl", Model.of("/api/getAfspraakDrempelOverzichtScreeningsOrganisatie?screeningsOrganisatieId=" + model.getObject().getId() + "&")));

		AjaxSubmitLink submitButton = new AjaxSubmitLink("submit")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				ScreeningOrganisatie screeningOrganisatie = model.getObject();
				instellingService.saveOrUpdateSoPlanningBk(screeningOrganisatie, ScreenitSession.get().getLoggedInInstellingGebruiker());
				BasePage.markeerFormulierenOpgeslagen(target);
				this.info("Gegevens zijn succesvol opgeslagen");
			}
		};
		submitButton.setVisible(magAanpassen);
		form.add(submitButton);

		add(form);
	}
}
