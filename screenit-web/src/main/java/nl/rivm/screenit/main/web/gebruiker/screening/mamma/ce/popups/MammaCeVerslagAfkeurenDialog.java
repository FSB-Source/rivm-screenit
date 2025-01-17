package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.popups;

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

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.panels.CeRadioloogZoekPanel;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;

public abstract class MammaCeVerslagAfkeurenDialog extends GenericPanel<MammaBeoordeling>
{

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	private CeRadioloogZoekPanel radioloogZoekPanel;

	public MammaCeVerslagAfkeurenDialog(String contentId, IModel<MammaBeoordeling> model)
	{
		super(contentId, model);

		ScreenitForm<MammaBeoordeling> form = new ScreenitForm<>("form", getModel());
		add(form);

		TextArea<String> textArea = new TextArea<>("afkeurreden");
		textArea.add(StringValidator.maximumLength(255));
		textArea.setRequired(true);
		form.add(textArea);
		radioloogZoekPanel = new CeRadioloogZoekPanel("zoekPanel", getModel())
		{
			@Override
			public void callback(AjaxRequestTarget target, IModel<InstellingGebruiker> radioloog)
			{
				createMedewerkerLijst();
				target.add(getMedewerkerLijstWrapper());
			}
		};
		add(radioloogZoekPanel);
		add(new IndicatingAjaxButton("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaBeoordeling beoordeling = ModelProxyHelper.deproxy(MammaCeVerslagAfkeurenDialog.this.getModelObject());
				beoordelingService.verslagAfkeurenDoorCE(beoordeling,
					radioloogZoekPanel.getSelectedGebruikerModel() != null ? radioloogZoekPanel.getSelectedGebruikerModel().getObject() : null,
					ScreenitSession.get().getLoggedInInstellingGebruiker());
				close(target);
			}
		});
	}

	public abstract void close(AjaxRequestTarget target);

}
