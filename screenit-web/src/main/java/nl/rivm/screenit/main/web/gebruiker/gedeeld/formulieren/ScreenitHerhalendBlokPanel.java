
package nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren;

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

import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Map;

import nl.topicuszorg.formulieren2.api.definitie.AntwoordKeuzeVraagDefinitie;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElement;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElementContainer;
import nl.topicuszorg.formulieren2.api.instantie.containers.HerhalendBlok;
import nl.topicuszorg.formulieren2.api.resultaat.Antwoord;
import nl.topicuszorg.formulieren2.api.resultaat.FormulierResultaat;
import nl.topicuszorg.formulieren2.beanantwoord.AbstractBeanAntwoord;
import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordVraagInstantie;
import nl.topicuszorg.formulieren2.beanantwoord.EnkelvoudigBeanAntwoord;
import nl.topicuszorg.formulieren2.persistence.dao.FormulierenPersistenceDao;
import nl.topicuszorg.formulieren2.wicketrenderer.HerhalingHelper;
import nl.topicuszorg.formulieren2.wicketrenderer.containers.OnBeforeEventSink;
import nl.topicuszorg.formulieren2.wicketrenderer.containers.herhalendblok.IHerhalendBlokPanel;
import nl.topicuszorg.formulieren2.wicketrenderer.events.OnBeforeRenderDone;
import nl.topicuszorg.formulieren2.wicketrenderer.interfaces.IWicketRenderContext;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AbstractDefaultAjaxBehavior;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.AjaxRequestTarget.IJavaScriptResponse;
import org.apache.wicket.ajax.AjaxRequestTarget.IListener;
import org.apache.wicket.ajax.attributes.AjaxRequestAttributes;
import org.apache.wicket.event.Broadcast;
import org.apache.wicket.markup.html.list.Loop;
import org.apache.wicket.markup.html.list.LoopItem;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ScreenitHerhalendBlokPanel extends GenericPanel<HerhalendBlok<?>> implements IHerhalendBlokPanel, OnBeforeEventSink
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ScreenitHerhalendBlokPanel.class);

	private final IModel<Integer> aantalPanels;

	private final IModel<FormulierResultaat> formulierResultaat;

	private final IModel<HerhalendBlok<?>> herhalendBlok;

	private final IWicketRenderContext context;

	private boolean triggerOnBeforeEventDone;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private FormulierenPersistenceDao formulierenPersistenceDao;

	public ScreenitHerhalendBlokPanel(String id, final IModel<HerhalendBlok<?>> herhalendBlok, final IModel<FormulierResultaat> formulierResultaat,
		final IWicketRenderContext context, int defaultHerhalingen)
	{
		super(id, herhalendBlok);
		this.formulierResultaat = formulierResultaat;
		this.herhalendBlok = herhalendBlok;
		this.context = context;
		this.aantalPanels = new Model<>(defaultHerhalingen);
		setOutputMarkupPlaceholderTag(true);

		add(new Loop("herhalingen", aantalPanels)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void populateItem(LoopItem item)
			{
				item.add(new InnerScreenitHerhalendBlokPanel("innerHerhaling", herhalendBlok, context, formulierResultaat, aantalPanels, item.getIndex())
				{

					private static final long serialVersionUID = 1L;

					@Override
					void onAddHerhaling(AjaxRequestTarget target)
					{
						updateHerhalingen(aantalPanels.getObject() + 1, target);
					}

					@Override
					void onRemoveHerhaling(AjaxRequestTarget target, int herhaling)
					{
						try
						{
							removeChildVragen(herhalendBlok.getObject(), herhaling);
							updateHerhalingen(aantalPanels.getObject() - 1, target);
						}
						catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
						{
							LOG.error("Er is een fout opgetreden!", e);
						}
					}

				});
			}
		});
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		try
		{
			getInitieleHerhaling(herhalendBlok.getObject());
		}
		catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
		{
			LOG.error("Er is een fout opgetreden!", e);
		}
	}

	@Override
	public void updateHerhalingen(int newHerhalingen, AjaxRequestTarget target)
	{
		if (newHerhalingen < aantalPanels.getObject())
		{
			for (int i = 0; i < aantalPanels.getObject() - newHerhalingen; i++)
			{
				try
				{
					removeChildVragen(herhalendBlok.getObject(), aantalPanels.getObject() - i);
				}
				catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e)
				{
					LOG.error("Er is een fout opgetreden!", e);
				}
			}
		}

		aantalPanels.setObject(newHerhalingen);
		if (target != null)
		{
			target.addListener(new IListener()
			{

				@Override
				public void onBeforeRespond(Map<String, Component> map, AjaxRequestTarget target)
				{
					for (Component component : map.values())
					{
						if (component instanceof OnBeforeEventSink)
						{
							OnBeforeEventSink onBeforeEventSink = (OnBeforeEventSink) component;
							onBeforeEventSink.triggerOnBeforeEventDone();
						}
					}
				}

				@Override
				public void onAfterRespond(Map<String, Component> map, IJavaScriptResponse response)
				{

				}

				@Override
				public void updateAjaxAttributes(AbstractDefaultAjaxBehavior behavior, AjaxRequestAttributes attributes)
				{

				}
			});
			target.add(this);
		}
	}

	protected boolean removeChildVragen(FormulierElementContainer<?> elementContainer, int herhaling)
		throws IllegalAccessException, InvocationTargetException, NoSuchMethodException
	{
		for (FormulierElement formulierElement : elementContainer.getElementen())
		{
			if (formulierElement instanceof BeanAntwoordVraagInstantie)
			{
				BeanAntwoordVraagInstantie beanAntwoordVraagInstantie = (BeanAntwoordVraagInstantie) formulierElement;

				for (Antwoord antwoord : formulierResultaat.getObject().getAntwoorden())
				{
					if (antwoord.getVraagInstantie().equals(beanAntwoordVraagInstantie))
					{
						AbstractBeanAntwoord beanAntwoord = (AbstractBeanAntwoord) antwoord;

						String collectionPropertyPath = beanAntwoord.getPropertyExpression().substring(0, beanAntwoord.getPropertyExpression().lastIndexOf("."));

						List collection = (List) PropertyUtils.getProperty(beanAntwoord.getRootObject(),
							collectionPropertyPath.substring(0, collectionPropertyPath.lastIndexOf('[')));
						if (collection.size() > herhaling)
						{
							Object removedObject = collection.remove(herhaling);
							if (removedObject instanceof HibernateObject)
							{
								HibernateObject hibernateObject = (HibernateObject) removedObject;
								if (hibernateObject.getId() != null)
								{
									hibernateService.delete(hibernateObject);
								}
							}
						}
						break;

					}
				}
				return true;
			}
			else if (formulierElement instanceof FormulierElementContainer)
			{
				FormulierElementContainer formulierElementContainer = (FormulierElementContainer) formulierElement;
				if (removeChildVragen(formulierElementContainer, herhaling))
				{
					return true;
				}
			}
		}

		return false;
	}

	private boolean getInitieleHerhaling(FormulierElementContainer<?> elementContainer) throws IllegalAccessException, InvocationTargetException, NoSuchMethodException
	{
		for (FormulierElement formulierElement : elementContainer.getElementen())
		{
			if (formulierElement instanceof BeanAntwoordVraagInstantie)
			{
				BeanAntwoordVraagInstantie beanAntwoordVraagInstantie = (BeanAntwoordVraagInstantie) formulierElement;

				if (!(beanAntwoordVraagInstantie.getVraagDefinitie() instanceof AntwoordKeuzeVraagDefinitie)
					|| !((AntwoordKeuzeVraagDefinitie) beanAntwoordVraagInstantie.getVraagDefinitie()).isMeervoudig())
				{
					Map<Integer, Integer> herhalingen = HerhalingHelper.bepaalHerhalingen(this);
					herhalingen.put(herhalingen.size(), 0);

					Antwoord antwoord = formulierenPersistenceDao.getAntwoordModel(context, formulierResultaat.getObject(), beanAntwoordVraagInstantie, herhalingen,
						beanAntwoordVraagInstantie.getVraagDefinitie().getAntwoordTypeClass());

					if (antwoord instanceof EnkelvoudigBeanAntwoord)
					{
						EnkelvoudigBeanAntwoord beanAntwoord = (EnkelvoudigBeanAntwoord) antwoord;

						if (beanAntwoord.getValue() != null)
						{
							String collectionPropertyPath = beanAntwoord.getPropertyExpression().substring(0, beanAntwoord.getPropertyExpression().lastIndexOf("."));
							List collection = (List) PropertyUtils.getProperty(beanAntwoord.getRootObject(),
								collectionPropertyPath.substring(0, collectionPropertyPath.lastIndexOf('[')));
							aantalPanels.setObject(collection.size());
							return true;
						}
					}
				}
			}
			else if (formulierElement instanceof FormulierElementContainer)
			{
				FormulierElementContainer formulierElementContainer = (FormulierElementContainer) formulierElement;
				if (getInitieleHerhaling(formulierElementContainer))
				{
					return true;
				}
			}
		}

		return false;
	}

	@Override
	public void triggerOnBeforeEventDone()
	{
		triggerOnBeforeEventDone = true;
	}

	@Override
	protected void onBeforeRender()
	{
		super.onBeforeRender();
		if (triggerOnBeforeEventDone)
		{
			send(this, Broadcast.BREADTH, new OnBeforeRenderDone());
		}
	}
}
