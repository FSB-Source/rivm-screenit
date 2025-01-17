package nl.rivm.screenit.main.web.gebruiker.login;

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

import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.security.IScreenitRealm;
import nl.rivm.screenit.service.AutorisatieService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.bootstrap.BootstrapFeedbackPanel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.Application;
import org.apache.wicket.Page;
import org.apache.wicket.RestartResponseException;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.CheckBoxMultipleChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class BvoSelectiePage extends LoginBasePage
{

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private IScreenitRealm realm;

	public BvoSelectiePage(InstellingGebruiker instellingGebruiker)
	{
		List<Bevolkingsonderzoek> onderzoeken = Bevolkingsonderzoek.sort(autorisatieService.getBevolkingsonderzoeken(instellingGebruiker));
		if (CollectionUtils.isNotEmpty(onderzoeken) && onderzoeken.size() == 1)
		{
			LOG.debug("Er is 1 BVO beschikbaar, deze wordt meteen toegekend.");
			add(new WebMarkupContainer("bvoBeschikbaarContainer").setVisible(false));
			add(new WebMarkupContainer("bvoOnbeschikbaarContainer").setVisible(false));
			instellingGebruiker.setBevolkingsonderzoeken(onderzoeken);
			hibernateService.saveOrUpdate(instellingGebruiker);
			realm.clearCachedAuthorizationInfo(instellingGebruiker);
			throw new RestartResponseException(ScreenitSession.get().getPageForInstellingGebruiker(instellingGebruiker));
		}
		else if (CollectionUtils.isNotEmpty(onderzoeken) && onderzoeken.size() > 1)
		{
			LOG.debug("Er zijn meerdere BVO's beschikbaar, selectiescherm wordt getoond.");
			WebMarkupContainer bvoBeschikbaarContainer = new WebMarkupContainer("bvoBeschikbaarContainer");
			Form<InstellingGebruiker> bvoForm = new Form<InstellingGebruiker>("bvoForm", ModelUtil.cModel(instellingGebruiker));
			bvoForm.add(new BootstrapFeedbackPanel("feedback"));
			CheckBoxMultipleChoice<Bevolkingsonderzoek> keuzemaken = new CheckBoxMultipleChoice<Bevolkingsonderzoek>("bevolkingsonderzoeken", onderzoeken,
				new EnumChoiceRenderer<Bevolkingsonderzoek>()
				{

					private static final long serialVersionUID = 1L;

					@Override
					public Object getDisplayValue(Bevolkingsonderzoek object)
					{
						return super.getDisplayValue(object) + " (" + object.getAfkorting() + ")";
					}
				});
			keuzemaken.setSuffix("<br>");
			bvoForm.add(keuzemaken);
			bvoForm.add(new AjaxSubmitLink("opslaan")
			{
				@Override
				protected void onSubmit(AjaxRequestTarget target)
				{
					InstellingGebruiker instGebruiker = (InstellingGebruiker) getForm().getDefaultModelObject();
					if (CollectionUtils.isEmpty(instGebruiker.getBevolkingsonderzoeken()))
					{
						error(getString("error.bvo.selecteer.een.minimaal"));
					}
					else
					{
						hibernateService.saveOrUpdate(instGebruiker);
						realm.clearCachedAuthorizationInfo(instGebruiker);
						setResponsePage(ScreenitSession.get().getPageForInstellingGebruiker(instGebruiker));
					}
				};
			});
			bvoBeschikbaarContainer.add(bvoForm);
			add(bvoBeschikbaarContainer);
			add(new WebMarkupContainer("bvoOnbeschikbaarContainer").setVisible(false));
		}
		else
		{
			LOG.debug("Er zijn geen BVO's beschikbaar, selectiescherm met uitleg wordt getoond.");
			add(new WebMarkupContainer("bvoBeschikbaarContainer").setVisible(false));
			WebMarkupContainer container = new WebMarkupContainer("bvoOnbeschikbaarContainer");
			container.add(new Link<Void>("stoppen")
			{
				@Override
				public void onClick()
				{
					Class<? extends Page> homePage = Application.get().getHomePage();
					ScreenitSession.get().logout();
					setResponsePage(homePage);
				}
			});
			add(container);
		}
	}
}
