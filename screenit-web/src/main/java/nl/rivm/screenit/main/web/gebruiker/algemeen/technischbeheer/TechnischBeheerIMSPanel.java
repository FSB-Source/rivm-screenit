package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.model.mamma.IMSConfiguratie;
import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.NumberTextField;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TechnischBeheerIMSPanel extends GenericPanel<IMSConfiguratie>
{
	private static final Logger LOG = LoggerFactory.getLogger(TechnischBeheerIMSPanel.class);

	@SpringBean
	private ParameterisatieService parameterisatieService;

	public TechnischBeheerIMSPanel(String imsConfiguratie, IModel<IMSConfiguratie> imsConfiguratieModel)
	{
		super(imsConfiguratie, CompoundPropertyModel.of(imsConfiguratieModel));
		createIMSConfigForm();
	}

	private void createIMSConfigForm()
	{
		Form<IMSConfiguratie> imsConfigForm = new Form<>("imsConfigForm");
		imsConfigForm.add(new TextField<>("hostName").setRequired(true).setEnabled(magAanpassen()));
		imsConfigForm.add(new NumberTextField<>("ormPort").setRequired(true).setEnabled(magAanpassen()));
		imsConfigForm.add(new NumberTextField<>("adtPort").setRequired(true).setEnabled(magAanpassen()));
		imsConfigForm.add(new NumberTextField<>("ilmPort").setRequired(true).setEnabled(magAanpassen()));

		imsConfigForm.add(new NumberTextField<>("bezwaarTermijnVerwijderdeBeelden").setRequired(true).setEnabled(magAanpassen()));
		imsConfigForm.add(new NumberTextField<>("imsQueueSizeWarningThreshold").setRequired(true).setEnabled(magAanpassen()));

		Component opslaanButton = createAndGetOpslaanButton();
		opslaanButton.setVisible(magAanpassen());
		imsConfigForm.add(opslaanButton);

		add(imsConfigForm);
	}

	private Component createAndGetOpslaanButton()
	{
		return new AjaxSubmitLink("submitIMSConfiguratie")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				opslaanIMSConfiguratieSettings(target);
				info("IMS configuratie is opgeslagen");
			}
		};
	}

	private void opslaanIMSConfiguratieSettings(AjaxRequestTarget target)
	{
		parameterisatieService.saveIMSConfiguratie(
			ScreenitSession.get().getLoggedInAccount(),
			getModelObject());
	}

	private boolean magAanpassen()
	{
		return ScreenitSession.get().checkPermission(Recht.TECHNISCH_BEHEER, Actie.AANPASSEN);
	}
}
