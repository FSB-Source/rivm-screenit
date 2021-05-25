package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.service.mamma.MammaPostcodeReeksService;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaPostcodeReeks;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaPostcodeReeksEditPage extends MammaPlanningBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(MammaPostcodeReeksEditPage.class);

	@SpringBean
	private MammaPostcodeReeksService postcodeReeksService;

	@SpringBean
	private MammaBaseStandplaatsService standplaatsService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private InstellingService instellingService;

	private BootstrapDialog dialog;

	private WebMarkupContainer mainContainer;

	public MammaPostcodeReeksEditPage(IModel<MammaPostcodeReeks> model)
	{
		setDefaultModel(model);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		addOrReplaceMainForm(null);

	}

	private void addOrReplaceMainForm(AjaxRequestTarget target)
	{
		WebMarkupContainer nieuwMainContainer = new WebMarkupContainer("mainContainer");
		nieuwMainContainer.setOutputMarkupId(true);
		ScreenitForm<MammaPostcodeReeks> mainForm = new ScreenitForm<>("mainForm", (IModel<MammaPostcodeReeks>) getDefaultModel());
		mainForm.setOutputMarkupId(true);
		nieuwMainContainer.add(mainForm);
		ComponentHelper.newPostcodeTextField(mainForm, "vanPostcode", true, !magAanpassen);
		ComponentHelper.newPostcodeTextField(mainForm, "totPostcode", true, !magAanpassen);

		mainForm.add(new ScreenitDropdown<>("standplaats",
			ModelUtil.listRModel(standplaatsService.getActieveStandplaatsen(ScreenitSession.get().getScreeningOrganisatie()), false),
			new ChoiceRenderer<MammaStandplaats>("naam")).setRequired(true).setEnabled(magAanpassen));

		addOpslaan(mainForm);
		addVerwijderenPostcodeReeks(mainForm);
		if (target == null)
		{
			mainContainer = nieuwMainContainer;
			add(mainContainer);
		}
		else
		{
			mainContainer.replaceWith(nieuwMainContainer);
			mainContainer = nieuwMainContainer;
			target.add(mainContainer);
		}
		add(new IndicatingAjaxLink<Void>("terug")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				terug(target, (IModel<MammaPostcodeReeks>) MammaPostcodeReeksEditPage.this.getDefaultModel());
			}

		});
	}

	protected void terug(AjaxRequestTarget target, IModel<MammaPostcodeReeks> model)
	{
		setResponsePage(MammaPostcodeReeksZoekenPage.class);
	}

	private void addOpslaan(ScreenitForm<MammaPostcodeReeks> mainForm)
	{
		mainForm.add(new IndicatingAjaxButton("opslaan")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaPostcodeReeks postcodeReeks = (MammaPostcodeReeks) getForm().getModelObject();
				if (postcodeReeks.getVanPostcode().compareToIgnoreCase(postcodeReeks.getTotPostcode()) > 0)
				{
					error(getString("vanGroterDanTotPostcode"));
				}
				else
				{
					String overlap = postcodeReeksService.overlaptBestaandeReeks(postcodeReeks);
					if (StringUtils.isNotBlank(overlap))
					{
						error(getString("overlap") + overlap);
					}
					String meerDanEenRegio = "";
					if (StringUtils.isNotBlank(meerDanEenRegio))
					{
						error(getString("meerDanEenRegio") + meerDanEenRegio);
					}
					if (!hasErrorMessage())
					{
						boolean changed = postcodeReeksService.saveOrUpdatePostcodeReeks(postcodeReeks, ScreenitSession.get().getLoggedInInstellingGebruiker());
						if (changed)
						{
							success(getString("message.gegevensopgeslagen"));
							target.add(mainForm);
							BasePage.markeerFormulierenOpgeslagen(target);
						}
					}
				}
			}
		}.setVisible(magAanpassen));
	}

	private void addVerwijderenPostcodeReeks(ScreenitForm<MammaPostcodeReeks> mainForm)
	{
		mainForm.add(new ConfirmingIndicatingAjaxLink<Void>("verwijderen", dialog, "question.postcodeReeks.verwijderen")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaPostcodeReeks postcodeReeks = mainForm.getModelObject();
				postcodeReeksService.deletePostcodeReeks(postcodeReeks, ScreenitSession.get().getLoggedInInstellingGebruiker());
				ScreenitSession.get().success(getString("postcodeReeks.verwijderd"));
				setResponsePage(MammaPostcodeReeksZoekenPage.class);
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(ingelogdNamensRegio && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.VERWIJDEREN)
					&& mainForm.getModelObject().getId() != null);
			}

		});
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return MammaPostcodeReeksZoekenPage.class;
	}
}
